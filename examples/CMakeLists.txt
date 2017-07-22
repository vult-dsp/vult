
cmake_minimum_required(VERSION 2.8)

# Adds functions to build PD externals
include(cmake/pd.cmake)

set(CMAKE_BUILD_TYPE Release)

set(CMAKE_MACOSX_RPATH 1)

# include of the Vult Runtime
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../runtime)

# include directories for Vult code
set(VULT_INCLUDES util osc midi env filters effects)

# OSCILLATORS

vult_pd(phase osc/phase.vult VULT_INCLUDES)
vult_pd(blit osc/blit.vult VULT_INCLUDES)
vult_pd(noise osc/noise.vult VULT_INCLUDES)
vult_pd(phd osc/phd.vult VULT_INCLUDES)
vult_pd(saw_eptr osc/saw_eptr.vult VULT_INCLUDES)
vult_pd(saw_ptr1 osc/saw_ptr1.vult VULT_INCLUDES)
vult_pd(saw_ptr2 osc/saw_ptr2.vult VULT_INCLUDES)
vult_pd(saw_r osc/saw_r.vult VULT_INCLUDES)
vult_pd(sawcore osc/sawcore.vult VULT_INCLUDES)
vult_pd(sine osc/sine.vult VULT_INCLUDES)
vult_pd(tricore osc/tricore.vult VULT_INCLUDES)

# MIDI

vult_pd(gates midi/gates.vult VULT_INCLUDES)
vult_pd(monocv midi/monocv.vult VULT_INCLUDES)
vult_pd(polycv midi/monocv.vult VULT_INCLUDES)

#ENVELOPES

vult_pd(ad env/ad.vult VULT_INCLUDES)
vult_pd(adsr env/adsr.vult VULT_INCLUDES)
vult_pd(ahr env/ahr.vult VULT_INCLUDES)
vult_pd(lfo env/lfo.vult VULT_INCLUDES)
vult_pd(swept env/swept.vult VULT_INCLUDES)

# FILTERS

vult_pd(ladder filters/ladder.vult VULT_INCLUDES)
vult_pd(svf filters/svf.vult VULT_INCLUDES)

# EFFECTS

vult_pd(saturate effects/saturate.vult VULT_INCLUDES)
vult_pd(saturate_soft effects/saturate_soft.vult VULT_INCLUDES)
vult_pd(short_delay effects/short_delay.vult VULT_INCLUDES)
vult_pd(bitcrush effects/bitcrush.vult VULT_INCLUDES)

# UNITS

vult_pd(kick units/kick.vult VULT_INCLUDES)
vult_pd(voice_4 units/voice_4.vult VULT_INCLUDES)
