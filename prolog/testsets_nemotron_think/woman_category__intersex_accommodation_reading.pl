% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the intersex_accommodation_reading of
 *   the woman_category kernel. It defines 'woman' as a biological category
 *   that acknowledges sex as a non-binary spectrum, including typical female
 *   biology plus intersex variations that do not fit the male category. This
 *   reading operates across legal, medical, and sports domains. In most
 *   policy domains (civil law, anti-discrimination, medical ethics),
 *   extractiveness is low — the category inclusion provides recognition and
 *   protection for a small population. In elite sports, however, the same
 *   category boundary becomes highly extractive: World Athletics and IOC
 *   regulations impose testosterone suppression on intersex women with
 *   differences of sex development (DSD) to compete in women's events,
 *   creating a tangled rope where inclusion in the woman category is the
 *   coordination function and mandatory medical intervention is the
 *   extraction. The Caster Semenya case (2009–present) is the paradigmatic
 *   instance. The measurement series captures the ratchet: pre-2009 minimal
 *   regulation, 2011 hyperandrogenism regulations, 2015 CAS suspension, 2018
 *   revised DSD regulations, 2023 further tightening. The 2024 end-point
 *   value (0.35) reflects the blended ε across domains — sports extraction is
 *   high but sports population is tiny; civil law extraction is near zero.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.35).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.45).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '757ea2dc-b019-4240-b625-7a4e44618c66').
narrative_ontology:cs_kernel_codification('757ea2dc-b019-4240-b625-7a4e44618c66', distributed).
narrative_ontology:cs_authority_grounding('757ea2dc-b019-4240-b625-7a4e44618c66', practice).
narrative_ontology:cs_interpretation_layer_present('757ea2dc-b019-4240-b625-7a4e44618c66').
narrative_ontology:cs_reading_relation('757ea2dc-b019-4240-b625-7a4e44618c66', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('757ea2dc-b019-4240-b625-7a4e44618c66', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('757ea2dc-b019-4240-b625-7a4e44618c66', foundational, biological_sex_is_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('757ea2dc-b019-4240-b625-7a4e44618c66', biological_sex_is_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('757ea2dc-b019-4240-b625-7a4e44618c66', foundational, intersex_variations_included_in_woman_category).
narrative_ontology:cs_axiom_status(intersex_variations_included_in_woman_category, holdable).
narrative_ontology:cs_axiom_grounding('757ea2dc-b019-4240-b625-7a4e44618c66', intersex_variations_included_in_woman_category, empirically_contingent).
narrative_ontology:cs_reference_frame('757ea2dc-b019-4240-b625-7a4e44618c66', binary_sex_classification_regime).
narrative_ontology:cs_drift_state('757ea2dc-b019-4240-b625-7a4e44618c66', contemporary_intersex_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('757ea2dc-b019-4240-b625-7a4e44618c66', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, typical_female_biology_women).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_female_typical_people).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_female_typical_people_in_sports).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_female_typical_people).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, biological_sex_nonbinary_spectrum).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, intersex_inclusion_in_woman_category).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, medical_autonomy_for_intersex_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their category membership as women is affirmed by this reading. They gain legal protections, sports access, and social recognition without new burdens. Exit is mobile — they can engage with or ignore the spectrum discourse without losing their category status.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, typical_female_biology_women, beneficiary,
    organized, biographical, mobile, global).

% In civil law and medical ethics, they gain category recognition and protection from non-consensual surgery — a genuine coordination benefit. In elite sports, they face mandatory testosterone suppression to compete, with no exit that preserves athletic career. Their intersex variation fuses with athletic identity: leaving competition means leaving the only context where their biology is relevant. This identity lock makes exit structurally unavailable, not merely costly.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_female_typical_people, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, intersex_female_typical_people, payer).

% Elite athletes with DSD (e.g., 46,XY 5-ARD) who compete in women's events. World Athletics DSD regulations require testosterone ≤ 2.5 nmol/L for 6+ months via medication or surgery. Non-compliance = ban from women's events. No alternative competition category exists. Exit means career termination. The extraction is medicalization as price of entry; the coordination story (fair competition) is the cover.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_female_typical_people_in_sports, payer,
    powerless, biographical, trapped, global).

% World Athletics, IOC, international federations. They write eligibility rules, control competition entry, and define 'fairness' for women's sport. They benefit from the woman category as a stable competitive class. They have arbitrage-grade exit: they can modify rules, create new categories, or shift enforcement costs to national bodies. Their institutional continuity depends on maintaining binary categories with narrow carve-outs.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Legislatures, courts, human rights bodies, medical boards. They adopt or reject spectrum definitions in anti-discrimination law, birth certificates, medical protocols. Some jurisdictions (Malta, Portugal, Iceland, some US states) have adopted non-binary or spectrum recognition. Others maintain binary. They face analytical exit: they can change policy without personal cost, but institutional inertia resists.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, legal_policy_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for binary sex classification (e.g., sex-based rights organizations, some feminist groups). They argue 'woman' must mean adult human female for sex-based protections. They are excluded from this reading's ontology but maintain parallel institutional presence. Exit is mobile — they advocate in legislatures, courts, and media without dependency on this constraint.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_advocates, excluded,
    organized, biographical, mobile, global).

% Advocates for self-identification as the sole criterion for womanhood. They view spectrum biology as still biologizing gender. Excluded from this reading's framework but politically adjacent. Exit is mobile — they pursue identity-based recognition independently.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, excluded,
    organized, biographical, mobile, global).

% Analyze the ethics of intersex medicalization, sports regulation, and category ontology. They do not collect rents or bear costs from the constraint. Their seat is the engine's analytical reference.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, bioethics_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a biologically grounded category definition that accommodates intersex variations without collapsing into identity-only or binary-only models. Solves the coordination problem of how law, medicine, and sport should classify people whose biology does not fit male/female binary — by expanding 'woman' to include female-typical intersex variations rather than creating a third category or defaulting to identity.
% TRANSFER_FUNCTION: In civil domains: transfers recognition, legal protection, and medical autonomy from binary-enforcing institutions to intersex people (gain for intersex people, cost to institutional inertia). In sports: transfers the burden of 'fairness' onto intersex women via mandatory medicalization — they pay with bodily autonomy and career viability; sports bodies collect legitimacy and binary category stability.
% ABSENT_VOICES: Intersex people with male-typical biology (excluded from 'woman' category under this reading — they would need a parallel 'man' accommodation reading). People who reject spectrum ontology entirely (would argue for binary in all domains). Infants subjected to non-consensual surgery (cannot yet speak). Future intersex people whose classification will be decided by today's institutional settlements.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, civil law would revert to binary sex classification in most jurisdictions (intersex people lose non-binary recognition, regain path to binary assignment). Sports would enforce strict binary with no carve-outs (intersex women banned from women's events or forced into men's events). Medical protocols would resume default assignment surgeries. The world rearranges because institutions currently use this reading (or its logic) to justify existing accommodations — removing it removes the justification.
% FOUNDING_PROBLEM: The binary sex classification system (male/female only) fails to accommodate intersex variations, causing: non-consensual infant genital surgeries to force binary conformity; legal exclusion from identity documents; sports bans for athletes who do not fit binary; medical pathologization of natural variation. This reading was built to solve that exclusion by expanding the woman category rather than multiplying categories.
% FOUNDING_PROBLEM_CORROBORATION: UN Human Rights Council (2019) resolution on intersex rights; Council of Europe (2017) resolution against non-consensual surgery; WHO (2024) guidance on intersex health; intersex-led organizations (OII Europe, InterACT, ILGA World) — all outside the beneficiary set of this reading (they advocate for intersex people, not for this specific ontology). Medical ethics literature (e.g., Creighton, Liao, Davis) corroborates the harm of binary enforcement.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.35) is a domain-blended value: near-zero in civil law/medical ethics (category inclusion is protective), very high in elite sports (testosterone suppression mandates). The blended value reflects population weighting — sports affects perhaps 5-10 elite athletes globally at any time, while civil recognition affects all intersex people. Suppression (0.45) captures active enforcement in sports (regulations, testing, bans) and softer enforcement in law (non-recognition of non-binary categories in many jurisdictions). Theater ratio (0.22) is low because the coordination function (legal recognition, medical ethics) is genuine, not performative. Accessibility collapse (0.52) is moderate: binary alternatives persist in sports and many legal systems, but spectrum recognition is spreading. Resistance (0.55) is moderate: intersex advocacy, human rights bodies, and some medical organizations resist sports regulations; sex biology advocates resist spectrum recognition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (intersex women in sports) and beneficiary seat (intersex women in civil law) are the same people in different domains. The engine should compute divergent types: rope/scaffold in civil domains (coordination with minimal extraction), snare/tangled_rope in sports (high extraction, active enforcement, suppressed exit). The agenda_setter seats (sports bodies, legal institutions) will compute as low-extraction beneficiaries of the coordination function. This seat divergence is the measurement — not an error to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Typical_female_biology_women are beneficiaries (d ≈ 0.15): category inclusion is affirmed, no extraction. Intersex_female_typical_people are dually positioned: in civil domains they are beneficiaries (d ≈ 0.2, category inclusion); in sports they are targets (d ≈ 0.9, mandatory medicalization, no exit from elite competition without abandoning career). Sports_governing_bodies are agenda_setters (d ≈ 0.1, they write and enforce rules, collect legitimacy). Legal_policy_institutions are agenda_setters (d ≈ 0.2, they administer category definitions). Sex_biology_advocates and gender_identity_advocates are excluded (d ≈ 0.7, their preferred definitions are not adopted, but they have exit to advocacy). Bioethics_scholars are observers (d = 0.5, analytical seat). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (binary sex classification harming intersex people) remains live: non-consensual infant surgeries continue, legal recognition is incomplete, sports bans are expanding. The arrangement has not outlived its function — but in sports, the coordination function (fair competition) has been captured by extraction (testosterone suppression as gatekeeping). The mandatrophy risk is domain-specific: civil recognition is not mandatrophic; sports regulation shows mandatrophy signals (theater rising, extraction rising, founding problem of 'fair competition' contested).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading (intersex_accommodation_reading) of the contested woman_category kernel. What structural classification do the sibling readings (sex_biology_reading, gender_identity_reading) receive, and how do their ε values differ?',
    'Author separate constraint stories for sex_biology_reading and gender_identity_reading with their own ε, beneficiaries, victims, and claimed_type. Link all three via network.affects_constraints.',
    'If sibling readings compute as different types (e.g., sex_biology_reading as mountain, gender_identity_reading as scaffold), the kernel itself has no single classification — only the readings do. This confirms the ε-invariance principle: the label ''woman category'' covers multiple constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment kernel decomposition: one kernel, three readings, three constraints').

omega_variable(
    sports_extraction_spike,
    'Why does extractiveness spike dramatically in elite sports (ε ≈ 0.75) while remaining low in most policy domains (ε ≈ 0.15)?',
    'Domain-specific measurement: compare enforcement intensity, stakeholder power asymmetry, and exit options in sports governing bodies vs. civil legal systems vs. medical protocols.',
    'If the spike is structural (sports bodies have monopoly power over competition entry + no exit for athletes), the constraint is a tangled_rope overall but functions as a snare in the sports subdomain. This may warrant a decomposed story: woman_category__intersex_accommodation_reading__sports_subdomain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_extraction_spike, empirical, 'Domain-conditioned extraction variance: low ε generally, high ε in sports').

omega_variable(
    binary_enforcement_challenge,
    'Does this reading''s spectrum model structurally foreclose binary enforcement, or does it create a dual-track system where binary categories persist in some domains (sports) while spectrum recognition operates in others (civil law)?',
    'Trace institutional adoption: which bodies have adopted spectrum definitions (some national laws, medical guidelines) and which maintain binary enforcement with carve-outs (World Athletics, IOC). Measure whether carve-outs are stable or shrinking.',
    'If dual-track is stable, this reading coexists_with sex_biology_reading institutionally. If spectrum recognition is displacing binary enforcement, this reading influences sex_biology_reading toward obsolescence. If binary enforcement reasserts (e.g., new sports bans), sex_biology_reading influences this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binary_enforcement_challenge, empirical, 'Structural relationship between spectrum and binary enforcement across institutional domains').

omega_variable(
    victim_beneficiary_duality,
    'Intersex_female_typical_people are listed as both beneficiaries (category inclusion) and victims (sports extraction). Is this a genuine tangled_rope structure (same agents coordinated and extracted), or are these distinct subpopulations?',
    'Disaggregate: do intersex women who never enter elite sports experience net benefit from category inclusion? Do elite intersex athletes experience net extraction despite category inclusion? Measure per-subpopulation χ.',
    'If the same individuals are both coordinated and extracted, tangled_rope is confirmed. If distinct subpopulations, the constraint may be a rope for most and a snare for athletes — warranting decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_beneficiary_duality, empirical, 'Whether beneficiary and victim roles attach to the same agents or different subpopulations').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by intersex women in sports structural (World Athletics regulations, eligibility bans) or internalized (medicalization pressure, identity negation), or both?',
    'Post-exit trajectory: do intersex athletes who leave elite sports still experience suppression in medical/legal contexts? If suppression persists after sports exit, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. This affects χ computation for identity_locked exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression for intersex women in sports').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2000, woman_category__intersex_accommodation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2005, woman_category__intersex_accommodation_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2009, woman_category__intersex_accommodation_reading, theater_ratio, 2009, 0.15).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2012, woman_category__intersex_accommodation_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2015, woman_category__intersex_accommodation_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2018, woman_category__intersex_accommodation_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2021, woman_category__intersex_accommodation_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_tr_t2024, woman_category__intersex_accommodation_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2000, woman_category__intersex_accommodation_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2005, woman_category__intersex_accommodation_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2009, woman_category__intersex_accommodation_reading, base_extractiveness, 2009, 0.25).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2012, woman_category__intersex_accommodation_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2015, woman_category__intersex_accommodation_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2018, woman_category__intersex_accommodation_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2021, woman_category__intersex_accommodation_reading, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_be_t2024, woman_category__intersex_accommodation_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2000, woman_category__intersex_accommodation_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2005, woman_category__intersex_accommodation_reading, suppression_requirement, 2005, 0.25).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2009, woman_category__intersex_accommodation_reading, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2012, woman_category__intersex_accommodation_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2015, woman_category__intersex_accommodation_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2018, woman_category__intersex_accommodation_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2021, woman_category__intersex_accommodation_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(woman_category__intersex_accommodation_reading_su_t2024, woman_category__intersex_accommodation_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__intersex_accommodation_reading, 0.08).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the woman_category kernel. The sex_biology_reading asserts binary chromosomal/anatomical definition (claimed_type: mountain in biology, tangled_rope in law). The gender_identity_reading asserts self-identification definition (claimed_type: scaffold in progressive law, rope in community practice). This reading asserts spectrum biology definition (claimed_type: tangled_rope overall). All three share the kernel but instantiate different constraints with different ε, beneficiaries, victims, and structural types. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, powerless, 0.85).
constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
