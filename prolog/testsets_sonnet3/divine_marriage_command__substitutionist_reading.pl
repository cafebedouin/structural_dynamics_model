% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Substitutionist Reading: Manifesto as Superseding Revelation Requiring Monogamy
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   Under sustained federal prosecution and threat of institutional
 *   dissolution, a religious institution's leadership issued a Manifesto
 *   ending the public practice of plural marriage. The substitutionist
 *   reading holds that this was not a policy retreat but an act of
 *   independent continuing revelation: the earlier command is understood to
 *   have been fulfilled and superseded, and monogamy is now the eternal,
 *   binding standard. This reading grounds the institution's ongoing
 *   legitimacy, its excommunication of fundamentalist groups who maintain the
 *   earlier command still binds, and its self-narrative of unbroken prophetic
 *   continuity. The reading is examined here as a constraint in its own
 *   right: what it extracts, from whom, and by what mechanism it is
 *   maintained.
 *
 * KEY AGENTS:
 *   - church_hierarchy: agenda_setter/beneficiary (institutional/arbitrage) — issues and enforces the superseding-revelation doctrine
 *   - fundamentalist_polygamist_splinter_groups: primary target (powerless/trapped) — reclassified from faithful to apostate by the same institution
 *   - plural_wives_and_children_of_dissolved_marriages: primary target (powerless/trapped) — bear costs of both the original command and its reversal
 *   - federal_government: excluded structural actor (institutional/analytical) — the coercive force the reading's narrative minimizes
 *   - religious_historians: analytical observer (analytical/analytical) — assesses the revelation-vs-policy question from documentary evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Substitutionist Reading: Manifesto as Superseding Revelation Requiring Monogamy").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '4c5d40cf-796f-4722-82db-f81fc53ef5f6').
narrative_ontology:cs_kernel_codification('4c5d40cf-796f-4722-82db-f81fc53ef5f6', formalized).
narrative_ontology:cs_authority_grounding('4c5d40cf-796f-4722-82db-f81fc53ef5f6', lineage).
narrative_ontology:cs_interpretation_layer_present('4c5d40cf-796f-4722-82db-f81fc53ef5f6').
narrative_ontology:cs_reading_relation('4c5d40cf-796f-4722-82db-f81fc53ef5f6', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('4c5d40cf-796f-4722-82db-f81fc53ef5f6', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('4c5d40cf-796f-4722-82db-f81fc53ef5f6', foundational, manifesto_constitutes_new_binding_revelation).
narrative_ontology:cs_axiom_status(manifesto_constitutes_new_binding_revelation, holdable).
narrative_ontology:cs_axiom_grounding('4c5d40cf-796f-4722-82db-f81fc53ef5f6', manifesto_constitutes_new_binding_revelation, theological).
narrative_ontology:cs_axiom('4c5d40cf-796f-4722-82db-f81fc53ef5f6', foundational, prior_plural_marriage_command_doctrinally_superseded).
narrative_ontology:cs_axiom_status(prior_plural_marriage_command_doctrinally_superseded, holdable).
narrative_ontology:cs_axiom_grounding('4c5d40cf-796f-4722-82db-f81fc53ef5f6', prior_plural_marriage_command_doctrinally_superseded, theological).
narrative_ontology:cs_axiom('4c5d40cf-796f-4722-82db-f81fc53ef5f6', secondary, continued_polygamy_constitutes_apostasy).
narrative_ontology:cs_axiom_status(continued_polygamy_constitutes_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('4c5d40cf-796f-4722-82db-f81fc53ef5f6', continued_polygamy_constitutes_apostasy, conventional).
narrative_ontology:cs_reference_frame('4c5d40cf-796f-4722-82db-f81fc53ef5f6', post_manifesto_monogamous_standard).
narrative_ontology:cs_drift_state('4c5d40cf-796f-4722-82db-f81fc53ef5f6', contemporary_scholarly_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4c5d40cf-796f-4722-82db-f81fc53ef5f6', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, post_manifesto_leadership_lineage).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_polygamist_splinter_groups).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, plural_wives_and_children_of_dissolved_marriages).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, excommunicated_believers).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the Manifesto as binding revelation, restructures doctrine to declare monogamy the eternal standard, and enforces compliance through excommunication proceedings. Gains institutional legitimacy, legal standing, and statehood negotiations by adopting the new reading; controls the narrative that frames the shift as revelation rather than concession.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, church_hierarchy, beneficiary).

% Accepts the monogamy standard as settled doctrine, gains social respectability and reduced federal persecution, and inherits a faith community reoriented around the new command. Their continued membership and social standing depend on accepting the substitutionist framing as truth rather than tactic.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, mainstream_membership, beneficiary,
    moderate, generational, constrained, national).

% Leaders who transition the institution through the doctrinal pivot secure their authority and successor legitimacy by anchoring it in claimed continuing revelation. Their prophetic credibility is retroactively strengthened by the reading that monogamy was always the destination, not a forced retreat.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, post_manifesto_leadership_lineage, beneficiary,
    institutional, civilizational, arbitrage, national).

% Continue practicing plural marriage on the belief that the original command was never rescinded, only suspended. Under the substitutionist reading they are recast as apostates rather than faithful adherents to an unrescinded command, and face excommunication, loss of community, and criminal prosecution. Their exit options are foreclosed by the same institution that once required the practice they are now punished for continuing.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_polygamist_splinter_groups, payer,
    powerless, generational, trapped, regional).

% Existing plural families lose legal and religious legitimacy overnight when the doctrine reverses; wives beyond the first are reclassified as non-marital partners, inheritance and standing are disrupted, and children born of plural unions inherit ambiguous legitimacy within the reorganized doctrine. They bore the costs of the original command and now bear the costs of its reversal, with no compensating benefit at either stage.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, plural_wives_and_children_of_dissolved_marriages, payer,
    powerless, biographical, trapped, local).

% Members who continue to hold that the original revelation is binding and unrescinded are formally cut off from the religious community that structured their entire social, economic, and spiritual life. Exit from the community was never a live option before the reversal; now remaining faithful to the earlier command is treated as the exit-worthy offense.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, excommunicated_believers, payer,
    powerless, biographical, trapped, local).

% Applied the coercive pressure (property seizure, imprisonment, statehood denial) that the substitutionist reading omits from its account of the doctrinal shift. Not part of the institution's internal theological conversation but structurally central to why the shift occurred when it did; the reading's success depends on this actor's role being minimized in the official narrative.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Examine documentary evidence — private correspondence, contemporaneous sermons, the timing relative to federal legislation — to assess whether the Manifesto reads as genuine new revelation or as retrospectively theologized policy reversal under duress.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution with a single, stable, forward-looking marital doctrine that can be taught uniformly, legally defended, and used to negotiate political normalization (statehood, property restoration, cessation of federal prosecution) — replacing an internally contested and externally besieged practice with unified doctrine.
% TRANSFER_FUNCTION: Moves legitimacy, legal safety, and institutional continuity to the mainstream church and its post-Manifesto leadership; moves social standing, family integrity, and community membership away from those who continue practicing or defending the superseded command.
% ABSENT_VOICES: The plural wives and children whose family status was retroactively delegitimized are not consulted in the doctrinal reframing; fundamentalist adherents who hold the original revelation was never rescinded are excluded from the room where the substitutionist framing is settled and are instead its subsequent targets.
% DISAPPEARANCE_RATIONALE: If the substitutionist reading were abandoned tomorrow in favor of a continuationist or coercion-visibility account, the institution's legal position, its narrative of unbroken prophetic authority, and its ongoing excommunication practice against fundamentalist groups would all require restructuring — the doctrinal architecture of the modern institution is built on this reading holding.
% FOUNDING_PROBLEM: The church faced federal prosecution, disincorporation, and denial of statehood over plural marriage; the founding problem this reading solves is reconciling continued claims of prophetic authority with abandonment of a practice that authority had explicitly commanded.
% FOUNDING_PROBLEM_CORROBORATION: The institution itself attests the Manifesto was independent revelation, citing the prophet's own account of divine instruction. Independent historians, examining the timing relative to the Edmunds-Tucker Act and contemporaneous private letters describing the decision as a survival necessity, along with descendants of fundamentalist practitioners who maintain the original command was never rescinded, corroborate a different reading from outside the benefiting institution — no source outside the church hierarchy and mainstream membership corroborates the pure-revelation account without qualification.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that the substitutionist reading's coordination function — unifying doctrine, enabling political normalization — is real, but it is purchased by transferring the entire cost of doctrinal reversal onto exactly the people who complied with the earlier command in good faith: plural wives whose marriages are retroactively delegitimized, and fundamentalists who are excommunicated for continuity rather than change. Suppression (0.72) is high because maintaining the reading requires active enforcement — excommunication proceedings, doctrinal correlation, control of institutional archives and narrative — against an alternative internal reading (continuationism) that never fully died out. Theater ratio (0.4) reflects that a substantial share of the doctrinal apparatus around the Manifesto's revelatory status serves narrative-legitimation rather than any operative religious function. Accessibility collapse (0.68) is high but not total: continuationist and coercion-visibility readings persist as minority positions, so alternatives have not fully collapsed, only been marginalized. Resistance (0.55) reflects sustained but minority internal dissent from fundamentalist groups who never accepted the substitutionist premise.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy and post-Manifesto leadership are structural beneficiaries: the reading secures their institutional survival, legal standing, and retroactive prophetic credibility, at essentially no cost to them (d near beneficiary end). Mainstream membership benefits secondarily through social respectability and reduced persecution. Fundamentalist splinter groups and dissolved plural families are structural targets: the same institution that once commanded their family structure now excommunicates them for maintaining it, with no exit that does not cost them community, family legitimacy, or legal standing (d near full-target end, trapped exit). The federal government is excluded from the internal directionality calculus entirely — it is neither beneficiary nor victim within the church's own reading, despite being structurally central to why the Manifesto occurred when it did.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling continued prophetic authority claims with abandonment of a divinely commanded practice) is genuinely contested in status: from the hierarchy's own account it is 'live' in the sense that the resolution (monogamy as the true standard) remains necessary doctrine; from an outside historical view the practical problem (federal prosecution) that occasioned the shift is long dead, and the doctrine has outlived the crisis that produced it, now serving primarily as narrative architecture rather than active crisis-management. This divergence between insider-live and outsider-dead status on the same founding problem is precisely the tangled-rope signature: a real coordination achievement (doctrinal and legal stabilization) sits on top of an unresolved extraction from those who complied with the superseded command and are now punished for that same compliance's continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_policy_authenticity,
    'Is the Manifesto''s claimed status as independent continuing revelation authentic prophetic experience, or is the substitutionist theological framing a retrospective legitimation layered onto a coerced policy reversal?',
    'This is the core disagreement the kernel''s three readings exist to hold open — resolution would require access to private revelatory experience unavailable to external verification; the best available evidence is documentary (private correspondence, sermon timing relative to federal legislative pressure, internal deliberation records) which the coercion_visibility_reading and continuationist_reading weigh differently than this substitutionist reading does.',
    'If documentary evidence strongly supports coercion-driven timing with the revelatory framing constructed after the fact, the substitutionist reading''s classification shifts toward snare (the revelation claim becomes cover for institutional survival extraction from fundamentalist adherents); if independent revelatory experience is credited on its own terms, the tangled_rope classification with genuine coordination function is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_policy_authenticity, conceptual, 'Whether the Manifesto''s revelatory status is authentic or retrospectively constructed legitimation — the central kernel-level dispute.').

omega_variable(
    excommunication_proportionality,
    'Does the severity of excommunication and social exclusion applied to fundamentalist adherents match a genuine doctrinal violation, or is it calibrated primarily to protect the institution''s political and legal standing?',
    'Comparative analysis of disciplinary severity applied to fundamentalist polygamy violations versus other doctrinal deviations of comparable theological weight within the same institution and period.',
    'If enforcement severity tracks political exposure rather than theological weight, this supports reading the enforcement machinery as extraction-protective rather than doctrine-protective, pushing the classification toward snare for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excommunication_proportionality, empirical, 'Whether excommunication severity is calibrated to doctrine or to institutional political risk.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the beneficiary/victim structure and extraction profile differ under the continuationist_reading or coercion_visibility_reading, given they are separate constraints?',
    'Not resolvable within this story by design (per the ε-invariance principle) — each sibling reading is authored as its own constraint file with its own ε and stakeholder structure; this omega documents that the delta exists and is significant rather than attempting to compute it here.',
    'Under continuationist_reading, fundamentalist groups would appear as the legitimate doctrinal continuity-holders rather than apostates, inverting much of the victim/beneficiary structure authored here; under coercion_visibility_reading, the federal government moves from excluded to a central coercive party, and the church hierarchy''s beneficiary status becomes explicitly conditional on survival-under-duress rather than revelatory legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents that sibling kernel readings produce structurally different constraints, per the ε-invariance decomposition rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__substitutionist_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(divi_tr_t50, divine_marriage_command__substitutionist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(divi_tr_t80, divine_marriage_command__substitutionist_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(divi_tr_t130, divine_marriage_command__substitutionist_reading, theater_ratio, 130, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__substitutionist_reading, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(divi_be_t50, divine_marriage_command__substitutionist_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(divi_be_t80, divine_marriage_command__substitutionist_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement(divi_be_t130, divine_marriage_command__substitutionist_reading, base_extractiveness, 130, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__substitutionist_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(divi_su_t50, divine_marriage_command__substitutionist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(divi_su_t80, divine_marriage_command__substitutionist_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(divi_su_t130, divine_marriage_command__substitutionist_reading, suppression_requirement, 130, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.1).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the divine_marriage_command kernel. continuationist_reading holds the original command remains doctrinally valid and the Manifesto is prudential suspension only; coercion_visibility_reading holds the Manifesto's legitimacy is openly grounded in survival-under-federal-coercion rather than independent revelation. All three readings share the same underlying historical event (the Manifesto) but diverge sharply on beneficiary/victim structure: this reading casts fundamentalist continuers as apostates and the hierarchy as revelatory beneficiaries, while continuationist_reading would cast the hierarchy's post-Manifesto suppression of polygamy as the extractive act against doctrinally faithful adherents, and coercion_visibility_reading would recenter the federal government as the primary coercive party with the hierarchy as a coerced-but-surviving institutional actor. Each story must be read as ε-invariant on its own terms; do not average or blend the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
