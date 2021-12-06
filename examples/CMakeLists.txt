
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

vult_pd(phase    ${CMAKE_CURRENT_SOURCE_DIR}/osc/phase.vult VULT_INCLUDES)
vult_pd(blit     ${CMAKE_CURRENT_SOURCE_DIR}/osc/blit.vult VULT_INCLUDES)
vult_pd(saw_blit ${CMAKE_CURRENT_SOURCE_DIR}/osc/saw_blit.vult VULT_INCLUDES)
vult_pd(noise    ${CMAKE_CURRENT_SOURCE_DIR}/osc/noise.vult VULT_INCLUDES)
vult_pd(phd      ${CMAKE_CURRENT_SOURCE_DIR}/osc/phd.vult VULT_INCLUDES)
vult_pd(saw_eptr ${CMAKE_CURRENT_SOURCE_DIR}/osc/saw_eptr.vult VULT_INCLUDES)
vult_pd(saw_ptr1 ${CMAKE_CURRENT_SOURCE_DIR}/osc/saw_ptr1.vult VULT_INCLUDES)
vult_pd(saw_ptr2 ${CMAKE_CURRENT_SOURCE_DIR}/osc/saw_ptr2.vult VULT_INCLUDES)
vult_pd(saw_r    ${CMAKE_CURRENT_SOURCE_DIR}/osc/saw_r.vult VULT_INCLUDES)
vult_pd(sawcore  ${CMAKE_CURRENT_SOURCE_DIR}/osc/sawcore.vult VULT_INCLUDES)
vult_pd(sine     ${CMAKE_CURRENT_SOURCE_DIR}/osc/sine.vult VULT_INCLUDES)
vult_pd(tricore  ${CMAKE_CURRENT_SOURCE_DIR}/osc/tricore.vult VULT_INCLUDES)
vult_pd(minblep  ${CMAKE_CURRENT_SOURCE_DIR}/osc/minblep.vult VULT_INCLUDES)

# MIDI

vult_pd(gates  ${CMAKE_CURRENT_SOURCE_DIR}/midi/gates.vult VULT_INCLUDES)
vult_pd(monocv ${CMAKE_CURRENT_SOURCE_DIR}/midi/monocv.vult VULT_INCLUDES)
vult_pd(polycv ${CMAKE_CURRENT_SOURCE_DIR}/midi/monocv.vult VULT_INCLUDES)

#ENVELOPES

vult_pd(ad    ${CMAKE_CURRENT_SOURCE_DIR}/env/ad.vult VULT_INCLUDES)
vult_pd(adsr  ${CMAKE_CURRENT_SOURCE_DIR}/env/adsr.vult VULT_INCLUDES)
vult_pd(ahr   ${CMAKE_CURRENT_SOURCE_DIR}/env/ahr.vult VULT_INCLUDES)
vult_pd(lfo   ${CMAKE_CURRENT_SOURCE_DIR}/env/lfo.vult VULT_INCLUDES)
vult_pd(swept ${CMAKE_CURRENT_SOURCE_DIR}/env/swept.vult VULT_INCLUDES)

# FILTERS

vult_pd(ladder ${CMAKE_CURRENT_SOURCE_DIR}/filters/ladder.vult VULT_INCLUDES)
vult_pd(svf    ${CMAKE_CURRENT_SOURCE_DIR}/filters/svf.vult VULT_INCLUDES)

# EFFECTS

vult_pd(saturate      ${CMAKE_CURRENT_SOURCE_DIR}/effects/saturate.vult VULT_INCLUDES)
vult_pd(saturate_soft ${CMAKE_CURRENT_SOURCE_DIR}/effects/saturate_soft.vult VULT_INCLUDES)
vult_pd(short_delay   ${CMAKE_CURRENT_SOURCE_DIR}/effects/short_delay.vult VULT_INCLUDES)
vult_pd(bitcrush      ${CMAKE_CURRENT_SOURCE_DIR}/effects/bitcrush.vult VULT_INCLUDES)
vult_pd(rescomb       ${CMAKE_CURRENT_SOURCE_DIR}/effects/rescomb.vult VULT_INCLUDES)
vult_pd(clipper       ${CMAKE_CURRENT_SOURCE_DIR}/effects/clipper.vult VULT_INCLUDES)
vult_pd(fold          ${CMAKE_CURRENT_SOURCE_DIR}/effects/fold.vult VULT_INCLUDES)

# UNITS

vult_pd(kick    ${CMAKE_CURRENT_SOURCE_DIR}/units/kick.vult VULT_INCLUDES)
vult_pd(voice_4 ${CMAKE_CURRENT_SOURCE_DIR}/units/voice_4.vult VULT_INCLUDES)
