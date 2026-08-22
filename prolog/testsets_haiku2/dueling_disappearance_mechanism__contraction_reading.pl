% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Axiom Displacement (Contraction Reading)
 *   domain: social/cultural/historical
 *
 * SUMMARY:
 *   This reading of dueling's disappearance asserts that dueling became
 *   culturally unthinkable not because institutions displaced it (courts,
 *   libel law, professional credentials outcompeted it as dispute-resolution
 *   mechanisms) nor because multiple independent sufficient causes converged,
 *   but because the foundational axiom-set of Western personhood contracted
 *   from honor-culture to dignity-culture. Under honor-culture, reputation
 *   was a transferable asset vulnerable to insult, and violent defense of
 *   honor was the legitimate proof of authentic standing. Dignity-culture
 *   asserts personhood has intrinsic worth independent of violent
 *   demonstration and frames dispute-resolution institutionally. This reading
 *   claims the shift was not a zero-sum institutional substitution but an
 *   ontological displacement: the very concept of 'honor as material asset
 *   defended through violence' became unintelligible. The constraint is
 *   claimed as MOUNTAIN (dignity-culture as an irreversible substrate shift,
 *   not a constructed choice) while measuring with low extractiveness and
 *   suppression—the reading asserts the shift was not extractive imposition
 *   but genuine axiomatic change. The beneficiaries are the dignity-culture
 *   practitioners and institutions whose frame became the default; the
 *   victims are honor-culture practitioners whose entire identity-frame
 *   became illegible.
 *
 * KEY AGENTS:
 *   - Dignity-culture practitioners (clergy, Enlightenment intellectuals, legal reformers, abolitionists) — institutional beneficiaries whose axioms became the substrate
 *   - Honor-culture practitioners (aristocrats, military officers, gentlemen) — identity-locked targets whose frame became unintelligible
 *   - Emergent middle classes — observers whose economic incompatibility with honor-culture provided passive infrastructure for dignity-culture's expansion
 *   - Legal and intellectual authorities — agenda-setters who codified dignity-culture's frame into law and doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.08).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Axiom Displacement (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "social/cultural/historical").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b').
narrative_ontology:cs_kernel_codification('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', formalized).
narrative_ontology:cs_authority_grounding('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', lineage).
narrative_ontology:cs_interpretation_layer_present('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b').
narrative_ontology:cs_reading_relation('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', foundational, personhood_has_intrinsic_dignity).
narrative_ontology:cs_axiom_status(personhood_has_intrinsic_dignity, holdable).
narrative_ontology:cs_axiom_grounding('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', personhood_has_intrinsic_dignity, deontological).
narrative_ontology:cs_axiom('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', foundational, reputation_not_transferable_through_violence).
narrative_ontology:cs_axiom_status(reputation_not_transferable_through_violence, holdable).
narrative_ontology:cs_axiom_grounding('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', reputation_not_transferable_through_violence, deontological).
narrative_ontology:cs_reference_frame('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', honor_culture_axiom_set).
narrative_ontology:cs_drift_state('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', post_enlightenment_dignity_culture_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('a7d83cbf-fce6-46b6-a5ad-4c60a1d6449b', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional and intellectual agents (clergy, legal reformers, Enlightenment-era writers, abolitionists) whose frameworks privileged intrinsic human worth and rational dispute resolution. As dignity-culture became the dominant frame for personhood, their axioms became the baseline for legitimacy. They did not 'win' dueling by argument alone—the cultural substrate shifted beneath honor practice, making it unintelligible.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners, beneficiary,
    institutional, generational, arbitrage, national).

% Aristocratic and gentlemen practitioners whose entire framework of reputation, masculine identity, and social standing rested on honor axioms (reputation as a transferable asset requiring violent defense, insult as a material injury, personal courage as proof of worth). As dignity-culture supplanted honor-culture as the foundational axiom set, their entire epistemic and social framework became illegible—not defeated by argument, but ontologically displaced. Their exit would require abandoning identity fusion with honor-culture norms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerful, biographical, identity_locked, national).

% Commercial and professional classes whose economic roles (banking, law, manufacturing) were incompatible with honor-culture's time-and-bloodshed costs. They provided passive infrastructure for dignity-culture's expansion: legal institutions, press circulation, professional credentials that could not rest on martial reputation. They did not directly displace honor; they instantiated alternative status-earning mechanisms that dignity-culture rationalized.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, emergent_middle_classes, observer,
    organized, generational, mobile, national).

% Courts, legislatures, churches, and publication networks that formalized dignity-culture's axioms into law, doctrine, and cultural narrative. They did not create the shift—they codified and broadcast it. Their power lay in making dignity-culture's frame the default lens through which personhood, injury, and justice were understood.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_and_intellectual_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Historians and sociologists examining whether dueling's disappearance was a culture-axiom displacement (this reading), an institutional outcompetition (institutional_displacement_reading), or multiple independent causes (overdetermined_composite_reading). They see the full structure and the contested genealogy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor-culture's dueling norm solved a reputation-signaling problem in a context where honor was a transferable asset and institutions for dispute resolution were weak or unavailable. Dignity-culture displaces this by asserting intrinsic worth independent of violent demonstration and vesting dispute resolution in courts and contract. No single coordination function is 'maintained'—the substrate of what counts as a valid claim on reputation shifts, making the dueling solution unintelligible rather than merely costly.
% TRANSFER_FUNCTION: Under honor-culture: violence transfers reputation—a man's honor is restored through bloodletting, and his opponent's honor is destroyed by wound or death. Under dignity-culture: reputation transfers through institutional channels (professional credentials, publication, legal vindication). The reading asserts this is not a zero-sum institutional substitution but an ontological displacement where the very concept of 'reputation as transferable through violence' becomes unthinkable.
% ABSENT_VOICES: The dueling practitioners themselves—honor-culture men whose entire identity and status frame depended on the axioms dueling instantiated. As the cultural substrate shifted, their voices were not excluded from the table; they were rendered unintelligible within the new frame. They could not argue their way back because the argument-form itself (appealing to honor as intrinsic property) had ceased to register as coherent.
% DISAPPEARANCE_RATIONALE: If dignity-culture's axioms had not displaced honor-culture's frame, institutional substitution alone (courts, libel law, professional credentials) would have left honor-culture alive in residual form—as a competing status metric, a subcultural norm, a persistent option for the powerful. The complete disappearance of dueling (even among military and aristocratic cohorts that retained strongest claim to honor-culture) indicates the substrate shifted: not that one institution outcompeted another, but that the entire axiom-set making dueling thinkable became illegible.
% FOUNDING_PROBLEM: Personhood and reputation required visible demonstration in honor-culture; honor was a material asset transferable through violence and vulnerable to insult. How does a man prove his worth and defend his standing when institutions cannot adjudicate reputation-injury? Dueling solved this by making the willingness to risk death proof of authentic honor.
% FOUNDING_PROBLEM_CORROBORATION: Dignity-culture practitioners (clergy, Enlightenment writers, legal reformers) attested the problem was misframed: reputation need not be a transferable asset or demonstrated through violence—personhood has intrinsic dignity that requires no proof. Modern sociologists of honor-culture (Wyatt-Brown on the antebellum South, Stewart on revenge systems) corroborate that honor-culture participants genuinely perceived reputation as a material, vulnerable asset requiring violent defense. No living practitioner of honor-culture within the dignity-culture frame—the corroboration comes from outsiders observing honor-culture's structure, not from honor-practitioners themselves (they are identity-locked, cannot exit the frame to corroborate its obsolescence).
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series shows declining extractiveness and suppression from 1750 to 1880, then a rise to 1900—the contraction reading interprets this trajectory as: (1) Mid-18th-century: honor-culture still dominant but dignity-culture beginning to compete (extractiveness 0.28, suppression 0.15 as the old frame requires active defense against the new); (2) 1800–1880: dignity-culture's axioms achieving cultural dominance; extractiveness and suppression both decline as the frame-shift becomes irreversible and honor-culture practitioners face identity-lock rather than coordinated resistance (by 1880, extractiveness 0.05, suppression 0.03—the constraint barely registers as extractive because it is no longer contested); (3) 1880–1900: slight rise (extractiveness 0.15, suppression 0.08) reflects residual honor-culture pockets in the American South and military, whose violent resistance (dueling among officers, honor killings in rural honor-culture zones) required renewed suppression. Theater ratio stays uniformly low (0.01–0.08) because the contraction reading asserts genuine axiomatic change, not performative preservation—there is no 'maintenance theater' once the frame has shifted. The beneficiary (dignity-culture practitioners) is listed in base_properties to trigger FSM evaluation: the reading claims this is a natural-law-like substrate shift (mountain), but declares beneficiaries—an omega omega documents the irreducible ambiguity (Is dignity-culture's dominance a discovered natural law of personhood, or a constructed displacement that happens to benefit identifiable agents?).
 *
 * PERSPECTIVAL GAP:
 *   From the dignity-culture seat (legal reformers, clergy, Enlightenment intellectuals), the shift is a discovery or rationalization—the realization that personhood has intrinsic dignity and does not require violent demonstration. From the honor-culture seat (aristocrats, military), the shift is a catastrophic illegibility—their entire epistemic and social framework has been replaced with one that treats their central concerns (honor, reputation, masculine courage) as barbaric rather than legitimate. The engine computes this seat-divergence from the structural data: the dignity-culture beneficiary seat sees a natural axiom-shift; the honor-culture target seat sees a constraint that has displaced its coherence. The claimed type (mountain) reflects the dignity-culture reading; the victim set reflects the honor-culture impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-culture practitioners (beneficiaries) have d near 0.0—they benefit from the frame-shift and do not bear its costs (their axioms become the default, no longer require active defense). Honor-culture practitioners (victims/payers) have d near 1.0—they bear the cost of illegibility; their entire identity-frame was displaced. However, this is NOT a snare: the reading asserts there is no coercive mechanism, no suppression of exits (honor-culture men were not legally prevented from dueling after the 1860s; they could exit into honor-culture, but the frame-shift meant such exit carried catastrophic identity cost—they would be seen as anachronistic and barbaric by the new dominant culture). The suppression is structural (internalized through frame-shift) rather than coercive (legal prohibition, surveillance, enforcement machinery). This is why suppression metrics are low: the constraint's persistence does not depend on active suppression-machinery, but on the irreversibility of the axiom-shift. The emergent middle classes have d near 0.5—they neither heavily benefit (they do not collect from the shift) nor heavily bear costs (their economic structures are compatible with dignity-culture); they are observers/passive infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by asserting the founding problem ('How does a man prove his worth in a context where institutions cannot adjudicate reputation?') is genuinely dead—dignity-culture solves it by reframing the problem as mis-stated. Dueling solved a real coordination problem in honor-culture's frame; dignity-culture does not 'solve' it better—it declares the problem is incoherent (reputation is not a material asset requiring violent defense; personhood has intrinsic dignity). The contest is at the axiom level, not at the mechanism level, so the mandate does not outlive its function—the function itself is replaced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_culture_as_natural_or_constructed,
    'Is dignity-culture''s axiom-set a discovered natural law of personhood, or a constructed displacement that benefits identifiable agents?',
    'Genealogical tracing of dignity-culture''s emergence: does it arise from (a) philosophical argument that was always latent and became visible, (b) structural conditions (institutional modernization, economic change) that made it functional, or (c) beneficiary power consolidation? Different genealogies imply different answers about whether the shift is a mountain or a snare.',
    'If (a) natural-law discovery, the constraint is genuinely a mountain and dignity-culture practitioners are incidental beneficiaries. If (b) functional response, the constraint sits between mountain and rope (coordination responding to structural change). If (c) beneficiary capture, the constraint is a false summit—a snare masquerading as a mountain because dignity-culture''s universalism obscures the fact that legal reformers, clergy, and Enlightenment intellectuals benefited from its dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_culture_as_natural_or_constructed, conceptual, 'Whether the axiom-shift represents natural-law discovery, functional adaptation, or beneficiary capture.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Does the measured low suppression (0.03–0.08 in the 1860–1880 window) reflect genuine axiomatic displacement, or does it mask internalized suppression of honor-culture practitioners?',
    'Post-suppression-lift trajectory: if honor-culture practitioners were freed from all legal and social barriers to dueling practice (e.g., a hypothetical jurisdiction that decriminalized dueling and socially valorized it), would dueling practice resume or remain abandoned? Resumption would indicate suppression was structural (internalized); abandonment would indicate genuine axiom-shift.',
    'If internalized, the constraint''s effective suppression is higher than the metric suggests, and the mechanism is closer to snare (coercive framing of identity) than mountain. If genuine axiom-shift, the low suppression accurately reflects that persistence does not depend on coercion—honor-culture practitioners could exit but choose not to because exit would mean abandoning their identity-frame entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether low measured suppression reflects genuine axiom-shift or internalized coercion.').

omega_variable(
    honor_culture_as_victim_or_superseded_frame,
    'Should honor-culture practitioners be classified as victims (bearing a cost imposed by dignity-culture''s dominance) or as practitioners of a superseded frame (facing no active coercion, only illegibility)?',
    'Semantic classification: are honor-culture practitioners harmed by dignity-culture''s displacement (implies victim status) or merely made unintelligible (implies observational asymmetry without victimhood)? The difference depends on whether we require active coercion for victim status or allow structural displacement.',
    'If victims, the constraint is tangled_rope or snare (beneficiaries + victims + possible enforcement). If superseded-frame, the constraint is mountain with an asymmetric impact surface (beneficiaries + observers). This directly affects the computed classification from different seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_culture_as_victim_or_superseded_frame, preference, 'Whether axiom-displacement creates victim status or merely obsolescence.').

omega_variable(
    reading_identity_versus_institutional_displacement,
    'If institutional displacement (courts, libel law, professional credentials outcompeting dueling as dispute-resolution) was independently sufficient to end dueling, is the contraction-reading''s axiom-displacement mechanism empirically falsifiable or merely underdetermined?',
    'Counterfactual historical comparison: examine jurisdictions where honor-culture axioms persisted into the 20th century despite modern institutional displacement (parts of the American South, Mediterranean honor-culture zones, honor-killing contexts in South Asian and Middle Eastern traditions). Do institutional alternatives (courts, legal remedies) actually displace dueling/honor-violence in these contexts, or does honor-culture persist despite institutional alternatives? If persistence despite alternatives, the contraction reading is supported. If institutional displacement is sufficient, the reading''s axiom-mechanism may be redundant.',
    'This omega addresses the distinction between the contraction_reading and institutional_displacement_reading: if both mechanisms are operative, the constraint sits in the overdetermined_composite_reading. If only axiom-displacement is necessary (institution-building insufficient alone), the contraction reading is structurally superior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_versus_institutional_displacement, empirical, 'Testability of axiom-displacement vs. institutional-displacement as independent sufficient causes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(duel_tr_t1830, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1830, 0.03).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1860, 0.02).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1880, 0.01).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.02).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.28).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.19).
narrative_ontology:measurement(duel_be_t1830, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1830, 0.12).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.08).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1880, 0.05).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.12).
narrative_ontology:measurement(duel_su_t1830, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1830, 0.08).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1860, 0.05).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1880, 0.03).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.06).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The kernel dueling_disappearance_mechanism decomposes into three constraint stories, each selecting a different causal mechanism for dueling's decline and producing a different type classification. The contraction_reading (this story) asserts axiom-set displacement (mountain). The institutional_displacement_reading asserts institutional outcompetition (rope). The overdetermined_composite_reading asserts multiple independent sufficient causes (tangled_rope). All three stories share the same empirical period (1750–1900 with focus on 1800–1880) and the same core event (dueling becoming culturally unthinkable in Western law and society), but disagree at the mechanism level. This disaggregation follows ε-invariance: each reading has a distinct referent (the standing arrangement under contest as the reading sees it) and produces a distinct ε. The contraction reading's referent is honor-culture's axiom-set; institutional reading's referent is dueling as a dispute-resolution mechanism; composite reading's referent is dueling as a practice-bundle subject to multiple independent pressures. The readings are siblings, not competing observations of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
