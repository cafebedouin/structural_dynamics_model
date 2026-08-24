% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite Reading: Overdetermined Decline)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Honor violence legitimacy — the social and sometimes legal recognition of
 *   ritualized violence (dueling, honor killings, vendetta) as a legitimate
 *   response to insult — declined in the West between roughly 1750 and 1900.
 *   This composite reading argues the decline was overdetermined: two
 *   distinct mechanisms operated simultaneously. The 'drop' mechanism:
 *   external costs rose (state prosecution, professional disqualification,
 *   changing military technology, bourgeois moral pressure). The
 *   'contraction' mechanism: the concept of honor itself was redefined from
 *   'willingness to kill/die for reputation' to 'integrity, professional
 *   reputation, moral character,' making violence structurally unthinkable
 *   rather than merely costly. The contraction edge is decisive: even if
 *   external costs had dropped to zero, the practice would not have returned
 *   because the conceptual framework that made it meaningful was gone. The
 *   two mechanisms had different victim sets (drop: traditional practitioners
 *   blocked by costs; contraction: those whose identity was bound to the old
 *   honor concept) and different extractiveness profiles (drop:
 *   suppression-heavy; contraction: identity-extraction-heavy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.62).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.68).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite Reading: Overdetermined Decline)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '9ed30b8d-e580-4302-a194-144d2e550830').
narrative_ontology:cs_kernel_codification('9ed30b8d-e580-4302-a194-144d2e550830', distributed).
narrative_ontology:cs_authority_grounding('9ed30b8d-e580-4302-a194-144d2e550830', practice).
narrative_ontology:cs_interpretation_layer_present('9ed30b8d-e580-4302-a194-144d2e550830').
narrative_ontology:cs_reading_relation('9ed30b8d-e580-4302-a194-144d2e550830', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('9ed30b8d-e580-4302-a194-144d2e550830', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('9ed30b8d-e580-4302-a194-144d2e550830', foundational, honor_violence_decline_overdetermined).
narrative_ontology:cs_axiom_status(honor_violence_decline_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('9ed30b8d-e580-4302-a194-144d2e550830', honor_violence_decline_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('9ed30b8d-e580-4302-a194-144d2e550830', foundational, contraction_renders_drop_insufficient).
narrative_ontology:cs_axiom_status(contraction_renders_drop_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('9ed30b8d-e580-4302-a194-144d2e550830', contraction_renders_drop_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('9ed30b8d-e580-4302-a194-144d2e550830', traditional_honor_violence_legitimacy).
narrative_ontology:cs_drift_state('9ed30b8d-e580-4302-a194-144d2e550830', post_dueling_abolition, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('9ed30b8d-e580-4302-a194-144d2e550830', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, traditional_aristocrats).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, military_officers).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, bourgeois_professionals).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_targeted_by_honor_violence).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, lower_class_men_excluded).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, conscientious_objectors_to_dueling).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, colonial_subjects_under_imposed_codes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, religious_authorities).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, military_officers).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, peer_enforcement_superior_to_state_law).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, violence_as_honest_signal_of_commitment).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, honor_as_prepolitical_social_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and enforced the codes duello; their status depended on willingness to kill or die for reputation. The decline of dueling legitimacy eroded their distinctive conflict-resolution monopoly and the identity framework that organized their social world. Exit meant abandoning the self-concept of 'gentleman' — professionally and socially fatal.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, traditional_aristocrats, agenda_setter,
    powerful, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, traditional_aristocrats, beneficiary).

% Bound by regimental honor codes that mandated dueling for officer corps cohesion. Paid with lives and careers; benefited from the coordination of trust among peers. As state militaries professionalized, the duel became a career-ending liability rather than a cohesion mechanism — exit was constrained by institutional discipline and peer enforcement.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, military_officers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, military_officers, payer).

% Gained from the redefinition of honor toward professional reputation, integrity, and commercial trustworthiness. The old duel culture was a barrier to their social ascent; its decline opened status pathways. They did not run the old system but captured the gains of its conceptual contraction.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, bourgeois_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Bore the sharpest extraction: honor killings, forced marriages, seclusion, and reputation destruction administered by male kin under the legitimacy of honor codes. Had no voice in the codes, no exit from the kinship structures that enforced them, and no alternative protection — state law often recognized 'honor' as mitigating circumstance.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_targeted_by_honor_violence, payer,
    powerless, biographical, trapped, local).

% Excluded from the duel as a status ritual (not a 'gentleman') but subject to honor violence — brawls, vendettas, and summary punishment by elites. The code's legitimacy licensed violence against them while denying them its protections. No exit from the spatial and economic dependency on local patrons.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, lower_class_men_excluded, payer,
    powerless, biographical, trapped, local).

% Men of standing who refused to duel on religious or moral grounds (Quakers, evangelicals, early liberals). Paid with social ostracization, professional exclusion, and 'posting' (public shaming). Exit from the honor code meant exit from their class world — constrained by identity and livelihood.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, conscientious_objectors_to_dueling, payer,
    moderate, biographical, constrained, regional).

% Colonial administrations codified 'customary' honor codes (e.g., tribal jirga, caste panchayat) as indirect rule tools, freezing fluid practices into rigid violence-legitimizing structures. Subjects bore the extraction of both traditional and colonial-enforced honor violence with no exit from either.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, colonial_subjects_under_imposed_codes, payer,
    powerless, generational, trapped, continental).

% Initially tolerated dueling as elite self-governance; progressively criminalized it (prosecution of seconds, anti-dueling laws, military codes). The constraint's decline was partly their doing — they built the external costs (drop mechanism). But they also relied on honor's legitimacy for social order in early period — dual position shifted over time.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, state_legal_authorities, observer).

% Churches both blessed the old honor (aristocratic Christianity) and drove its redefinition (evangelical 'manliness' without violence, Catholic sacramental confession replacing blood atonement). Gained moral authority from the contraction; lost influence over elite males who saw religion as incompatible with the duel.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, religious_authorities, beneficiary).

% Analyze the constraint from outside — no stake in its operation or decline. Their readings (drop, contraction, composite) are the analytical seats whose divergence this story models.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a closed-system, peer-enforced conflict resolution mechanism for status groups where state courts were experienced as too slow, too public, and inadequate for the specific good of 'reputation among equals.' The duel settled disputes without state intervention, maintaining group cohesion and hierarchy.
% TRANSFER_FUNCTION: Moved the risk of death, injury, and social standing from the insulted party to the insulter through ritualized violence; moved the authority to define legitimate violence from the state to the peer group; moved the burden of social control from impersonal law to personal reputation. Women and lower-class men paid the externalities — their bodies and mobility were the collateral of the peer group's coordination.
% ABSENT_VOICES: Women subjected to honor violence (killed, confined, married off) — structurally silenced by kinship authority and legal recognition of 'honor defense.' Lower-class men excluded from the duel but punished by its logic — no access to the codes duello, no seconds, no legitimacy. Colonial subjects under imposed 'customary' honor codes — the codes were fossilized by colonial administrators who claimed to preserve tradition while rigidifying it for control. None of these groups were in the room when the codes were written or when they were abolished.
% DISAPPEARANCE_RATIONALE: When dueling legitimacy vanished, elite conflict resolution migrated to courts, press, and parliament; the concept of honor shifted from 'willingness to kill' to 'professional integrity and moral character'; gender relations reorganized as women's 'honor' ceased to be a male kin property; colonial 'customary' codes persisted in distorted form. The world rearranged — the constraint's disappearance was not a return to a natural state but a structural transformation.
% FOUNDING_PROBLEM: Early modern elites faced a coordination problem: state courts could not adjudicate 'insult' (too subjective, too public, too slow), yet unresolved insults destroyed the trust necessary for military command, political alliance, and commercial credit among peers. The duel provided a binding, private, peer-enforced settlement mechanism that preserved the face of both parties and the cohesion of the group.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Robert Shoemaker on English courts, Victor Kiernan on dueling's social function) and sociologists of honor (Julian Pitt-Rivers, Frank Henderson Stewart, William Ian Miller) — writing from outside the aristocratic beneficiary class — concur that the founding problem (absence of adequate state adjudication for reputation disputes) was substantially solved by 19th-century legal reforms, press regulation, and professional reputation systems. The arrangement persisted decades after its founding problem died.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at mid-interval) reflects the constraint's dual nature: it coordinated elite conflict resolution (rope function) while extracting from women, lower-class men, conscientious objectors, and colonial subjects (snare function). Suppression (0.68) was high because the code required active enforcement — seconds, witnesses, social ostracization of refusers — and because state suppression of alternatives (courts for honor disputes) was incomplete early on. Theater ratio rises from 0.18 to 0.85 across the interval: early duels were functional; late duels were increasingly performative 'demonstrations of principle' with deliberately non-lethal outcomes. Accessibility collapse (0.58) is moderate — legal alternatives existed but were structurally inaccessible for honor disputes (too public, wrong remedy). Resistance (0.52) grew from scattered religious objection to organized legal and moral campaigns.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the traditional_aristocrat seat, the constraint appears as rope (genuine coordination they built and maintained, worth the cost); from the women_targeted seat, it appears as snare (pure extraction, no coordination function for them); from the bourgeois_professional seat, it appears as piton (atrophied coordination they neither built nor needed, persisting by inertia). The composite reading holds that ALL these perceptions are structurally real simultaneously — the constraint was a tangled rope whose different faces were experienced by different agents. The engine's per-seat computation is the measurement of this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional aristocrats and military officers are identity-locked targets (d→1.0): their self-concept fused with the code, exit meant social death. They were also agenda-setters who administered the constraint — a dual position that the engine will compute as high effective extraction despite their beneficiary role. Bourgeois professionals are mobile beneficiaries (d→0.2): they gained from the redefinition without bearing the old costs. Women and lower-class men are trapped payers (d→1.0): no exit, no voice, maximal extraction. Conscientious objectors are constrained payers (d→0.8): exit existed but at prohibitive identity cost. State authorities shift from observer to agenda-setter over the interval — the derivation captures this via time_horizon and power change, but the snapshot at mid-interval shows them as institutional agenda-setters with analytical exit. Colonial subjects are trapped payers at continental scope — the worst structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (elite need for private reputation adjudication) died by ~1850 as courts, press, and professional bodies provided alternatives. But the constraint persisted for decades — a classic mandatrophy. The theater ratio rise (0.18→0.85) tracks this: the coordination function atrophied, the performance remained. The contraction mechanism accelerated mandatrophy resolution by making the old practice conceptually impossible, not just practically costly. The drop mechanism alone would have left a dormant constraint ready to revive if costs fell; contraction killed the revival path. Mandatrophy is resolved — the arrangement is gone — but the conceptual redefinition that resolved it extracted from those whose identity was the old honor (identity_locked payers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Is the overdetermined decline (composite) the structurally true account, or does one mechanism (drop or contraction) dominate such that the other is epiphenomenal?',
    'Counterfactual simulation: if external costs (drop) had been removed but conceptual redefinition (contraction) proceeded, would dueling have revived? If contraction had been blocked but costs rose, would the practice have gone underground rather than vanished? Historical natural experiments (e.g., Southern US vs. Northern Europe; colonial vs. metropole trajectories) provide partial evidence.',
    'If composite is true, the constraint''s classification must reflect two simultaneous extraction profiles with different victim sets — a tangled rope with a contraction edge that creates irreversible identity-level extraction. If drop dominates, the constraint is a snare that weakened but could revive. If contraction dominates, it''s a mountain-like conceptual collapse with extraction as byproduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the kernel''s decline was genuinely overdetermined or monocausal, and where the structural disagreement between readings lies.').

omega_variable(
    contraction_vs_drop_victim_sets,
    'Are the victim sets of the drop and contraction mechanisms genuinely distinct, or do they overlap substantially?',
    'Prosopographic analysis of who resisted the decline and how: traditional aristocrats who dueled illegally (drop victims) vs. traditional aristocrats who internalized the new honor concept and suffered identity rupture (contraction victims). Memoirs, correspondence, and court records can separate these populations.',
    'If victim sets are distinct, the composite constraint has a bifurcated extraction structure requiring dual-seat analysis. If they overlap, the two mechanisms extract from the same agents through different channels — still overdetermined but with simpler seat structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_vs_drop_victim_sets, empirical, 'Distinctness of victim populations across the two decline mechanisms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of dueling primarily structural (state law, professional codes) or internalized (moral conviction that dueling was wrong), and how did this shift over the interval?',
    'Track the language of anti-dueling arguments: early phase emphasizes legal penalty and social cost (structural); late phase emphasizes sin, dishonor, and unmanliness (internalized). The proportion of each at each phase indicates the suppression mechanism mix.',
    'If suppression was largely internalized by the end, the constraint''s effective suppression is higher than structural measures suggest — agents carried the suppression with them after the external enforcement faded. This affects the theater_ratio interpretation and the piton/mountain boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism across the decline interval.').

omega_variable(
    colonial_honor_codes_fossilization,
    'Did colonial administration freeze honor violence legitimacy in colonized societies, preventing the contraction mechanism from operating there?',
    'Compare trajectories: metropole (both mechanisms) vs. colonies (drop imposed by colonial law, contraction blocked by colonial codification of ''custom''). Post-colonial persistence of honor violence in fossilized forms tests this.',
    'If true, the colonial_subjects stakeholder experiences a different constraint variant — a snare maintained by external power without the contraction release valve. This would require a separate constraint story linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_honor_codes_fossilization, empirical, 'Whether colonial rule structurally altered the decline mechanism for subject populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvl_comp_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hvl_comp_tr_t25, honor_violence_legitimacy__composite_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(hvl_comp_tr_t50, honor_violence_legitimacy__composite_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(hvl_comp_tr_t75, honor_violence_legitimacy__composite_reading, theater_ratio, 75, 0.48).
narrative_ontology:measurement(hvl_comp_tr_t100, honor_violence_legitimacy__composite_reading, theater_ratio, 100, 0.62).
narrative_ontology:measurement(hvl_comp_tr_t125, honor_violence_legitimacy__composite_reading, theater_ratio, 125, 0.75).
narrative_ontology:measurement(hvl_comp_tr_t150, honor_violence_legitimacy__composite_reading, theater_ratio, 150, 0.85).

% Extraction over time
narrative_ontology:measurement(hvl_comp_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(hvl_comp_be_t25, honor_violence_legitimacy__composite_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(hvl_comp_be_t50, honor_violence_legitimacy__composite_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(hvl_comp_be_t75, honor_violence_legitimacy__composite_reading, base_extractiveness, 75, 0.55).
narrative_ontology:measurement(hvl_comp_be_t100, honor_violence_legitimacy__composite_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(hvl_comp_be_t125, honor_violence_legitimacy__composite_reading, base_extractiveness, 125, 0.28).
narrative_ontology:measurement(hvl_comp_be_t150, honor_violence_legitimacy__composite_reading, base_extractiveness, 150, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hvl_comp_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hvl_comp_su_t25, honor_violence_legitimacy__composite_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(hvl_comp_su_t50, honor_violence_legitimacy__composite_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(hvl_comp_su_t75, honor_violence_legitimacy__composite_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(hvl_comp_su_t100, honor_violence_legitimacy__composite_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement(hvl_comp_su_t125, honor_violence_legitimacy__composite_reading, suppression_requirement, 125, 0.45).
narrative_ontology:measurement(hvl_comp_su_t150, honor_violence_legitimacy__composite_reading, suppression_requirement, 150, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, state_monopoly_violence).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, professional_reputation_systems).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, colonial_customary_law).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, gender_guardianship_laws).

% DUAL FORMULATION NOTE:
% This composite_reading decomposes the honor_violence_legitimacy kernel into two simultaneous mechanisms (drop and contraction) with distinct victim sets and extractiveness profiles. The drop_reading and contraction_reading are sibling constraints that each capture one mechanism as if it were the whole story. The composite shows each is incomplete alone: drop without contraction leaves a revivable constraint; contraction without drop leaves a conceptual shift without the enforcement that made it stick. All three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
