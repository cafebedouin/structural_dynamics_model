% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone as Commemorative Husk (Decayed Land-Use Commitment)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In 1933, survivors of the Showa Sanriku tsunami in the hamlet of Aneyoshi
 *   erected a stone marker inscribed with a warning not to build homes below
 *   its elevation. After the 2011 Tohoku tsunami, the marker became
 *   internationally famous as a story of ancestral wisdom saving lives. This
 *   story instantiates the COMMEMORATIVE HUSK reading of the contested
 *   kernel: that by the time of 2011, the stone's original behavioral
 *   constraint on land-use had already decayed into a symbolic/memorial
 *   object with no actual enforcement power over where people built, and that
 *   survival in 2011 is better explained by timing, wave geometry, and
 *   evacuation behavior than by multi-generational compliance with the
 *   stone's directive. The sibling reading (behavioral_competence_reading, a
 *   separate constraint file) holds the opposite: that the stone retained
 *   genuine operational force across 78 years. Both readings share the same
 *   physical artifact and historical record but diverge sharply on what that
 *   record shows about actual land-use decisions relative to the marker line
 *   — this is the defining ε-invariance case for committer-frame
 *   decomposition: same text, same object, two different constraints
 *   depending on which behavioral claim is being evaluated.
 *
 * KEY AGENTS:
 *   - aneyoshi_residents_1933_founders: original agenda-setters, now deceased, whose intent is unrecoverable
 *   - aneyoshi_residents_below_marker: bear the actual risk under this reading regardless of the stone's symbolic status
 *   - municipal_tourism_office: primary beneficiary of the museum-piece function
 *   - disaster_memorial_narrative_industry: collects attention/credibility from the prophetic-wisdom story
 *   - local_government_planning_authority: excluded from formalizing or repudiating the marker's force
 *   - disaster_researchers: analytical observers assessing the land-use record independent of the narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.71).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.28).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone as Commemorative Husk (Decayed Land-Use Commitment)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'ecd9e4af-d64a-463d-bd52-aa52ced8fb21').
narrative_ontology:cs_kernel_codification('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', fixed_text).
narrative_ontology:cs_authority_grounding('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', practice).
narrative_ontology:cs_reading_relation('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', foundational, commemorative_function_independent_of_behavioral_compliance).
narrative_ontology:cs_axiom_status(commemorative_function_independent_of_behavioral_compliance, holdable).
narrative_ontology:cs_axiom_grounding('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', commemorative_function_independent_of_behavioral_compliance, empirically_contingent).
narrative_ontology:cs_axiom('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', secondary, survival_causation_underdetermined_by_marker_proximity).
narrative_ontology:cs_axiom_status(survival_causation_underdetermined_by_marker_proximity, holdable).
narrative_ontology:cs_axiom_grounding('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', survival_causation_underdetermined_by_marker_proximity, empirically_contingent).
narrative_ontology:cs_reference_frame('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', founders_original_siting_directive).
narrative_ontology:cs_drift_state('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', post_2011_media_canonization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ecd9e4af-d64a-463d-bd52-aa52ced8fb21', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_tourism_office).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memorial_narrative_industry).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_below_marker).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_property_purchasers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Survivors of the 1933 Showa Sanriku tsunami erected the stone after losing nearly the entire village, carving a warning against building below its line. They set the original directive but have no mechanism to enforce it on descendants; their authority is inherited only as much as later generations choose to honor it, and it decays with every generation that does not personally remember the wave.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_1933_founders, agenda_setter,
    powerless, generational, trapped, local).

% Later households built homes and infrastructure below the stone's marked line, treating the marker as heritage rather than instruction. Some in this reading survived March 2011 by chance of timing, evacuation speed, or wave-path geometry rather than because siting decisions honored the stone; the stone imposed no actual constraint on where they built or lived, and its failure to function as a rule is invisible to them until or unless a wave arrives.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_below_marker, payer,
    powerless, biographical, trapped, local).

% Buyers and builders who acquired or developed land below the marker treated the stone as a historical curiosity rather than a zoning constraint, since no building code, insurance requirement, or municipal ordinance referenced it. Their land-use decisions were made on price, view, and access grounds entirely independent of the stone's directive; they bear the residual physical risk the stone was meant to warn against.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_property_purchasers, payer,
    moderate, biographical, constrained, local).

% Promotes the stone as a landmark of disaster memory and resilience folklore, drawing visitors, media coverage, and disaster-preparedness tourism revenue after 2011 reframed the stone as a prophetic success story. Benefits from the narrative regardless of whether the stone actually constrained anyone's building decisions; the commemorative value is independent of, and in this reading disconnected from, any behavioral compliance record.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_tourism_office, beneficiary,
    institutional, generational, arbitrage, regional).

% Journalists, documentary producers, and disaster-preparedness educators cite Aneyoshi as an exemplary case of intergenerational wisdom successfully guiding survival. They collect attention, funding, and platform relative to the constraint's mythic value; this reading holds that the underlying causal claim (stone directive caused survival) is substantially overstated relative to what the land-use record shows.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memorial_narrative_industry, beneficiary,
    organized, generational, mobile, national).

% Never incorporated the stone's marked elevation into zoning maps, building permits, or hazard ordinances. Would have standing to formalize the stone's line as a binding setback but has not been drawn into that conversation; its absence from the process is precisely what allowed the stone to decay from rule to symbol without anyone having to repeal it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_government_planning_authority, excluded,
    institutional, biographical, analytical, regional).

% Study the Aneyoshi case as a test of whether inscribed disaster memory retains operational force across generations without institutional reinforcement. Under this reading, they find land-use records showing construction below the marker predating 2011, weakening the popular narrative that the stone functioned as a live behavioral constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally, the stone coordinated a single generational act of collective memory-making after catastrophic loss — a physical, durable warning meant to transmit a land-use rule across generations who would not personally remember the wave. That coordination function is the claim under contest in this reading.
% TRANSFER_FUNCTION: In this reading, the arrangement transfers reputational and commercial value (tourism revenue, media narrative capital, disaster-preparedness credibility) from the actual land-use risk borne by residents and purchasers below the marker to institutions that curate and monetize the stone's symbolism, without those institutions bearing any of the residual physical risk.
% ABSENT_VOICES: The 1933 carvers, now all deceased, cannot attest whether they intended the stone as binding instruction or communal grief marker; their intent is unrecoverable and is filled in retrospectively by whichever reading is convenient. Local planning authorities who could formalize or repudiate the marker's force are not part of the commemorative conversation at all.
% DISAPPEARANCE_RATIONALE: If the stone were removed, tourism and narrative revenue would visibly diminish (the museum-piece function would end), but under this reading no actual land-use pattern would change, because none was ever governed by the stone in practice — construction below the marker already occurred while the stone stood. Whether 'the world rearranges' therefore depends entirely on which reading is correct, which is exactly the contest this story instantiates one side of.
% FOUNDING_PROBLEM: The 1933 tsunami killed the overwhelming majority of Aneyoshi's population; survivors needed a durable, illiterate-accessible, multi-generational mechanism to prevent future settlement from repeating the fatal siting choice, since institutional memory and written records were expected to fade or be lost.
% FOUNDING_PROBLEM_CORROBORATION: Tourism and memorial-narrative institutions attest the founding problem was solved and the stone remains functionally protective, citing 2011 survival. Independent land-use and hazard-mapping researchers outside those benefiting institutions report construction records showing settlement below the marked line predating 2011, and note the absence of any binding ordinance derived from the stone — corroboration for the 'decayed to symbol' reading comes from parties with no commercial stake in the memorial narrative.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) under this reading because value (tourism revenue, narrative capital, preparedness-education credibility) is extracted from a symbol whose underlying behavioral claim is, in this reading, false or substantially overstated — the extraction rides on a coordination story (ancestral wisdom protected the village) that this reading holds did not actually operate as claimed. Theater ratio is very high (0.82) and rising sharply after 2011 (0.55 to 0.78 between 2011 and 2015) precisely because the stone's public function shifted almost entirely to performative commemoration once it became a media object, while its actual behavioral footprint on land use (already faded, per this reading, well before 2011) did not change at all. Suppression is comparatively low (0.28) because no one is coerced into avoiding the risk zone — the low suppression is itself part of the diagnostic: a genuine live behavioral rule would show some suppression mechanism (permitting refusal, social sanction against building low); its near-absence here is evidence for the husk reading. Accessibility collapse is moderate-low (0.35): alternative land-use narratives (the stone as memorial rather than mandate) were never actually foreclosed, they were simply overwritten by a more marketable story after 2011.
 *
 * PERSPECTIVAL GAP:
 *   From the tourism/narrative-industry seat, the stone is a living testament to intergenerational wisdom — a rope, coordinating memory across time. From the seat of residents whose actual building decisions were made on ordinary economic grounds (price, access, view) with no reference to the marker, the same object is inert stone that happens to have a good story attached after the fact. The engine should compute these seats differently given the divergent structural data authored per seat; the divergence itself is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents below the marker and coastal purchasers are payers/targets: they bear the residual physical risk that the stone was originally meant to prevent, without receiving any of the narrative or commercial value generated by the stone's fame. The tourism office and memorial-narrative industry are beneficiaries with mobile or arbitrage-grade exit — they can relocate attention to a different exemplary case if the Aneyoshi story is ever debunked, at no personal risk. The 1933 founders are neither beneficiaries nor targets in the present tense; they are the agenda-setters whose authority has decayed to the point of irrelevance to actual present-day land-use decisions, which is the central claim of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent repeat catastrophic siting below the tsunami line) is, under this reading, DEAD as a live constraint by the time it mattered most — not because the problem itself resolved, but because the mechanism meant to solve it (inscribed multi-generational warning with no institutional reinforcement) lost operational force well before 2011. What persists afterward is not the original coordination function but its commemorative shell, maintained because the shell has independent value (tourism, narrative capital) unrelated to whether the underlying land-use protection ever worked. This is the piton signature: no concentrated beneficiary captures land-use compliance (none exists to capture), diffuse cost sits on residents below the marker, and what remains is mostly performance — a memorial industry that could not, structurally, un-perform the story even if the land-use record were more widely known.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_use_record_vs_marker_line,
    'Does the actual historical record of building permits, structure locations, and settlement patterns in Aneyoshi between 1933 and 2011 show construction occurring below the stone''s marked elevation, or does it show consistent avoidance of that zone across the full interval?',
    'Archival review of municipal building records, land registry data, and aerial/cadastral survey history for the Aneyoshi hamlet across the 78-year interval, cross-referenced against the stone''s marked elevation line.',
    'If the record shows sustained avoidance, this reading is falsified and the sibling behavioral_competence_reading is the structurally accurate constraint. If the record shows construction below the line predating 2011, this reading is corroborated and the popular ''ancestral wisdom saved the village'' narrative is substantially a post-hoc commemorative construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_use_record_vs_marker_line, empirical, 'Whether the historical land-use record supports or contradicts the husk reading''s core claim.').

omega_variable(
    survival_causal_attribution_2011,
    'Was 2011 survival in the vicinity of the marker attributable to residents'' siting decisions tracking the stone''s directive, or to independent factors (evacuation speed, wave-path geometry, time of day, individual choices unrelated to the marker)?',
    'Reconstruction of individual household evacuation and structure-location histories for 2011, compared against distance from the marker line and against comparable non-marker villages with similar tsunami exposure.',
    'Determines whether the widely circulated international media narrative is empirically grounded or a compelling but causally loose post-disaster story — directly bears on whether the constraint''s present commemorative value is honestly earned or extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_causal_attribution_2011, empirical, 'Causal attribution question underlying the survival narrative this reading contests.').

omega_variable(
    founders_original_intent_unrecoverable,
    'Did the 1933 carvers intend the stone as a binding, generationally-enforced land-use rule, or primarily as a grief memorial and general admonition without expectation of literal multi-generational compliance?',
    'None fully available — the carvers are deceased and left no supplementary written record of intent beyond the inscription itself; oral history from immediate descendants (now also elderly or deceased) is the only remaining partial source and is itself filtered through 90 years of narrative reinterpretation.',
    'If original intent was purely memorial/admonitory rather than a strict siting rule, then the ''decay'' framing in this reading''s own title is itself contestable — there may be nothing to decay from if no binding rule was ever intended, which would reframe this reading as the historically accurate one rather than a decayed version of the sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founders_original_intent_unrecoverable, conceptual, 'Whether the founding intent was ever a binding behavioral rule at all, which bears on how to interpret the word ''decay'' in this reading''s own framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1955, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(aney_tr_t1975, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(aney_tr_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.55).
narrative_ontology:measurement(aney_tr_t2015, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2015, 0.78).
narrative_ontology:measurement(aney_tr_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2024, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement(aney_be_t1955, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1955, 0.22).
narrative_ontology:measurement(aney_be_t1975, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(aney_be_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.61).
narrative_ontology:measurement(aney_be_t2015, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(aney_be_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2024, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint and aneyoshi_stone_commitment__behavioral_competence_reading are the two readings of the aneyoshi_stone_commitment kernel. They share the same physical artifact (the 1933 tsunami stone) and the same headline historical fact (the 2011 tsunami, the marker's post-2011 fame) but diverge on the underlying behavioral claim: whether the marker functioned as a live, compliance-enforced land-use rule across 78 years (behavioral_competence_reading, likely rope or scaffold with low theater_ratio) or decayed into pure commemorative symbolism with no actual land-use constraint (this story, piton-flavored with high theater_ratio and substantial extractiveness). Per the ε-invariance principle, these are two distinct constraints, not one constraint measured two ways — their ε values differ by a wide margin and they carry different beneficiary/victim structures. A resolution of the land_use_record_vs_marker_line omega in either file would provide strong evidence against the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
