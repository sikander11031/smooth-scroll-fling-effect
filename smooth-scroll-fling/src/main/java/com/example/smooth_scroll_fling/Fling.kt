package com.example.smooth_scroll_fling


import androidx.compose.animation.core.AnimationState
import androidx.compose.animation.core.DecayAnimationSpec
import androidx.compose.animation.core.FloatDecayAnimationSpec
import androidx.compose.animation.core.animateDecay
import androidx.compose.animation.core.generateDecayAnimationSpec
import androidx.compose.foundation.gestures.FlingBehavior
import androidx.compose.foundation.gestures.ScrollScope

import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Density
import kotlin.math.abs
import kotlin.math.exp
import kotlin.math.ln
import kotlin.math.sign

object Fling{


    @Composable
    fun smoothScroll(): FlingBehavior = flingBehavior(

        scrollConfiguration = FlingConfiguration.Builder()
            .build()
    )

}


@Composable
fun flingBehavior(
    scrollConfiguration: FlingConfiguration =
        FlingConfiguration.Builder()
            .build()
): FlingBehavior {
    val flingSpec = rememberSplineBasedDecay<Float>(scrollConfiguration)
    return remember(flingSpec) {
        FlingerFlingBehavior(flingSpec)
    }
}
class FlingerFlingBehavior(
    private val flingDecay: DecayAnimationSpec<Float>
) : FlingBehavior {
    override suspend fun ScrollScope.performFling(initialVelocity: Float): Float {
        // come up with the better threshold, but we need it since spline curve gives us NaNs
        return if (abs(initialVelocity) > 1f) {
            var velocityLeft = initialVelocity
            var lastValue = 0f
            AnimationState(
                initialValue = 0f,
                initialVelocity = initialVelocity,
            ).animateDecay(flingDecay) {
                val delta = value - lastValue
                val consumed = scrollBy(delta)
                lastValue = value
                velocityLeft = this.velocity
                // avoid rounding errors and stop if anything is unconsumed
                if (abs(delta - consumed) > 1.5f) this.cancelAnimation()
            }
            velocityLeft
        } else {
            initialVelocity
        }
    }
}

@Composable
fun <T> rememberSplineBasedDecay(scrollConfiguration: FlingConfiguration): DecayAnimationSpec<T> {
    // This function will internally update the calculation of fling decay when the density changes,
    // but the reference to the returned spec will not change across calls.
    val density = LocalDensity.current
    return remember(density.density) {
        SplineBasedFloatDecayAnimationSpec(
            density,
            scrollConfiguration
        ).generateDecayAnimationSpec()
    }
}


internal fun computeSplineInfo(
    splinePositions: FloatArray,
    splineTimes: FloatArray,
    nbSamples: Int,
    flingConfiguration: FlingConfiguration
) {
    var xMin = 0.0f
    var yMin = 0.0f
    for (i in 0 until nbSamples) {
        val alpha = i.toFloat() / nbSamples
        var xMax = 1.0f
        var x: Float
        var tx: Float
        var coef: Float
        while (true) {
            x = xMin + (xMax - xMin) / 2.0f
            coef = 3.0f * x * (1.0f - x)
            tx = coef * ((1.0f - x)
                    * flingConfiguration.splineP1 + x
                    * flingConfiguration.splineP2) + x * x * x
            if (abs(tx - alpha) < 1E-5) break
            if (tx > alpha) xMax = x else xMin = x
        }
        splinePositions[i] =
            coef * ((1.0f - x) * flingConfiguration.splineStartTension + x) + x * x * x
        var yMax = 1.0f
        var y: Float
        var dy: Float
        while (true) {
            y = yMin + (yMax - yMin) / 2.0f
            coef = 3.0f * y * (1.0f - y)
            dy = coef * ((1.0f - y) * flingConfiguration.splineStartTension + y) + y * y * y
            if (abs(dy - alpha) < 1E-5) break
            if (dy > alpha) yMax = y else yMin = y
        }
        splineTimes[i] = coef * ((1.0f - y) * flingConfiguration.splineP1 + y *
                flingConfiguration.splineP2
                ) + y * y * y
    }
    splineTimes[nbSamples] = 1.0f
    splinePositions[nbSamples] = splineTimes[nbSamples]
}

/**
 * This class defines all the fling related parameters that can be tweaked by the user.
 *
 * @author Joseph James.
 */
class FlingConfiguration private constructor(
    val scrollFriction: Float,
    val absVelocityThreshold: Float,
    val gravitationalForce: Float,
    val inchesPerMeter: Float,
    val decelerationFriction: Float,
    val decelerationRate: Float,
    val splineInflection: Float,
    val splineStartTension: Float,
    private val splineEndTension: Float,
    val numberOfSplinePoints: Int
) {

    val splineP1: Float by lazy { splineStartTension * splineInflection }

    val splineP2: Float by lazy { 1.0f - splineEndTension * (1.0f - splineInflection) }

    data class Builder(
        /*
         * This variable manages the friction to the scrolls in the LazyColumn
         */
        var scrollViewFriction: Float = 0.008f,

        /*
         * This is the absolute value of a velocity threshold, below which the
         * animation is considered finished.
         */
        var absVelocityThreshold: Float = 0f,

        /*
         * Gravitational obstruction to the scroll.
         */
        var gravitationalForce: Float = 9.80665f,

        /*
         * Scroll Inches per meter
         */
        var inchesPerMeter: Float = 39.37f,

        /*
         * Rate of deceleration of the scrollView.
         */
        var decelerationRate: Float = (ln(0.78) / ln(0.9)).toFloat(),

        /*
         * Friction at the time of deceleration.
         */
        var decelerationFriction: Float = 0.09f,

        /*
         * Inflection is the place where the start and end tension lines cross each other.
         */
        var splineInflection: Float = 0.1f,

        /*
         * Spline's start tension.
         */
        var splineStartTension: Float = 0.1f,

        /*
         * Spline's end tension.
         */
        var splineEndTension: Float = 1.0f,

        /*
         * number of sampling points in the spline
         */
        var numberOfSplinePoints: Int = 100
    ) {

        fun build() = FlingConfiguration(
            scrollViewFriction,
            absVelocityThreshold,
            gravitationalForce,
            inchesPerMeter,
            decelerationFriction,
            decelerationRate,
            splineInflection,
            splineStartTension,
            splineEndTension,
            numberOfSplinePoints
        )
    }
}

class SplineBasedFloatDecayAnimationSpec(
    density: Density,
    scrollConfiguration: FlingConfiguration
) :
    FloatDecayAnimationSpec {

    /*
     * Fling calculation.
     */
    private val flingCalculator = FlingCalculator(
        density = density,
        flingConfiguration = scrollConfiguration
    )

    /*
     * This is the absolute value of a velocity threshold, below which the animation is
     * considered finished.
     */
    override val absVelocityThreshold: Float by lazy { scrollConfiguration.absVelocityThreshold }

    /*
     * Distance that is to be covered by the fling.
     */
    private fun flingDistance(startVelocity: Float): Float =
        flingCalculator.flingDistance(startVelocity) * sign(startVelocity)

    /*
     * Target value at the end of the fling.
     */
    override fun getTargetValue(initialValue: Float, initialVelocity: Float): Float =
        initialValue + flingDistance(initialVelocity)

    @Suppress("MethodNameUnits")
    override fun getValueFromNanos(
        playTimeNanos: Long,
        initialValue: Float,
        initialVelocity: Float
    ): Float {
        val playTimeMillis = playTimeNanos / 1_000_000L
        return initialValue + flingCalculator.flingInfo(initialVelocity).position(playTimeMillis)
    }

    @Suppress("MethodNameUnits")
    override fun getDurationNanos(initialValue: Float, initialVelocity: Float): Long =
        flingCalculator.flingDuration(initialVelocity) * 1_000_000L

    @Suppress("MethodNameUnits")
    override fun getVelocityFromNanos(
        playTimeNanos: Long,
        initialValue: Float,
        initialVelocity: Float
    ): Float {
        val playTimeMillis = playTimeNanos / 1_000_000L
        return flingCalculator.flingInfo(initialVelocity).velocity(playTimeMillis)
    }
}
class AndroidFlingSpline(private val flingConfiguration: FlingConfiguration) {
    private val samples by lazy { flingConfiguration.numberOfSplinePoints }

    private val splinePositions by lazy {
        FloatArray(flingConfiguration.numberOfSplinePoints + 1)
    }

    private val splineTimes by lazy {
        FloatArray(flingConfiguration.numberOfSplinePoints + 1)
    }

    init {
        computeSplineInfo(splinePositions, splineTimes, samples, flingConfiguration)
    }

    /**
     * Compute an instantaneous fling position along the scroller spline.
     *
     * @param time progress through the fling animation from 0-1
     */
    fun flingPosition(time: Float): FlingResult {
        val index = (samples * time).toInt()
        var distanceCoef = 1f
        var velocityCoef = 0f
        if (index < samples) {
            val tInf = index.toFloat() / samples
            val tSup = (index + 1).toFloat() / samples
            val dInf = splinePositions[index]
            val dSup = splinePositions[index + 1]
            velocityCoef = (dSup - dInf) / (tSup - tInf)
            distanceCoef = dInf + (time - tInf) * velocityCoef
        }
        return FlingResult(
            distanceCoefficient = distanceCoef,
            velocityCoefficient = velocityCoef
        )
    }

    /**
     * The rate of deceleration along the spline motion given [velocity] and [friction].
     */
    fun deceleration(velocity: Float, friction: Float): Double =
        ln(flingConfiguration.splineInflection * abs(velocity) / friction.toDouble())

    /**
     * Result coefficients of a scroll computation
     */
    data class FlingResult(
        /**
         * Linear distance traveled from 0-1, from source (0) to destination (1)
         */
        val distanceCoefficient: Float,
        /**
         * Instantaneous velocity coefficient at this point in the fling expressed in
         * total distance per unit time
         */
        val velocityCoefficient: Float
    )
}

/**
 * Configuration for Android-feel flinging motion at the given density.
 *
 * @param density density of the screen. Use LocalDensity to get current density in composition.
 * @param flingConfiguration this contain all parameters need for setting the scroll behaviour.
 *
 * @author Joseph James
 */
class FlingCalculator(
    val density: Density,
    val flingConfiguration: FlingConfiguration
) {

    private val androidFlingSpline: AndroidFlingSpline by lazy {
        AndroidFlingSpline(flingConfiguration = flingConfiguration)
    }

    /**
     * Compute the rate of deceleration based on pixel density, physical gravity
     * and a [coefficient of friction][friction].
     */
    private fun computeDeceleration(friction: Float, density: Float): Float =
        flingConfiguration.gravitationalForce *
                flingConfiguration.inchesPerMeter *
                density *
                160f *
                friction


    /**
     * A density-specific coefficient adjusted to physical values.
     */
    private val magicPhysicalCoefficient: Float by lazy { computeDeceleration(density) }

    /**
     * Computes the rate of deceleration in pixels based on
     * the given [density].
     */
    private fun computeDeceleration(density: Density) =
        computeDeceleration(flingConfiguration.decelerationFriction, density.density)

    private fun getSplineDeceleration(velocity: Float): Double =
        androidFlingSpline.deceleration(
            velocity,
            flingConfiguration.scrollFriction * magicPhysicalCoefficient
        )

    /**
     * Compute the duration in milliseconds of a fling with an initial velocity of [velocity]
     */
    fun flingDuration(velocity: Float): Long {
        val l = getSplineDeceleration(velocity)
        val decelMinusOne = flingConfiguration.decelerationRate - 1.0
        return (1000.0 * exp(l / decelMinusOne)).toLong()
    }

    /**
     * Compute the distance of a fling in units given an initial [velocity] of units/second
     */
    fun flingDistance(velocity: Float): Float {
        val l = getSplineDeceleration(velocity)
        val decelMinusOne = flingConfiguration.decelerationRate - 1.0
        return (
                flingConfiguration.scrollFriction * magicPhysicalCoefficient
                        * exp(flingConfiguration.decelerationRate / decelMinusOne * l)
                ).toFloat()
    }

    /**
     * Compute all interesting information about a fling of initial velocity [velocity].
     */
    fun flingInfo(velocity: Float): FlingInfo {
        val l = getSplineDeceleration(velocity)
        val decelMinusOne = flingConfiguration.decelerationRate - 1.0
        return FlingInfo(
            initialVelocity = velocity,
            distance = (
                    flingConfiguration.scrollFriction * magicPhysicalCoefficient
                            * exp(flingConfiguration.decelerationRate / decelMinusOne * l)
                    ).toFloat(),
            duration = (1000.0 * exp(l / decelMinusOne)).toLong(),
            androidFlingSpline
        )
    }

    /**
     * Info about a fling started with [initialVelocity]. The units of [initialVelocity]
     * determine the distance units of [distance] and the time units of [duration].
     */
    data class FlingInfo(
        val initialVelocity: Float,
        val distance: Float,
        val duration: Long,
        val androidFlingSpline: AndroidFlingSpline
    ) {
        fun position(time: Long): Float {
            val splinePos = if (duration > 0) time / duration.toFloat() else 1f
            return distance * sign(initialVelocity) *
                    androidFlingSpline.flingPosition(splinePos).distanceCoefficient
        }

        fun velocity(time: Long): Float {
            val splinePos = if (duration > 0) time / duration.toFloat() else 1f
            return androidFlingSpline.flingPosition(splinePos).velocityCoefficient *
                    sign(initialVelocity) * distance / duration * 1000.0f
        }
    }
}
