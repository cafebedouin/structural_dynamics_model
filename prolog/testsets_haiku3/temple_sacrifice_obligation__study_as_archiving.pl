% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation: Study as Archiving (Post-Temple Reading)
 *   domain: religious/halakhic authority
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, the obligation to
 *   perform sacrificial rites became impossible — no Temple, no altar, no
 *   priesthood authorized to perform these commandments. Yet halakhic
 *   authority maintained the binding status of the obligation. Under the
 *   'study-as-archiving' reading, study of sacrifice law preserves the
 *   knowledge and procedures for future restoration (when the Temple is
 *   rebuilt in the messianic age) but does NOT fulfill the present
 *   obligation. This reading structures the entire post-Temple period as
 *   non-compliance: the obligation remains binding, performable knowledge is
 *   archived through study, and Israel bears the cost of maintaining
 *   unfulfilled commandment. The constraint is CLAIMED as tangled_rope
 *   (coordination of textual transmission + binding obligation) while the
 *   metrics reflect high extractiveness (binding without fulfillment =
 *   perpetual asymmetry), moderate suppression (bindingness enforced through
 *   halakhic authority structure), and growing theater (the study ritual
 *   performs knowledge-preservation rather than actual sacrifice). This is
 *   one reading of the contested kernel 'temple_sacrifice_obligation'; its
 *   siblings are 'study_as_occupation' (study fulfills the obligation) and
 *   'messianic_suspension' (obligation is suspended, not violated). The
 *   kernel contest is halakhic — different Jewish interpretive traditions
 *   instantiate different structural understandings of the same divine
 *   commandment.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure: Interprets halakhic law, maintains binding status of obligation, enforces study as the mandatory mode of preservation. Benefits from centralized interpretive power over an unperformable obligation.
 *   - collective_israel: Bears the status of unfulfilled commandment; obligated to study sacrifice law but unable to perform it. Victim of the binding-without-fulfillment asymmetry.
 *   - textual_transmission_institutions: Yeshivas, academies, scribal communities that preserve and transmit sacrifice knowledge. Beneficiary of the framing that makes their work mandatory.
 *   - priestly_descendants: Descendants of the original sacrificial priesthood (Kohanim); their service role is nominally preserved in the study obligation but substantively unfulfillable. Mixed position: beneficiary of preserved status, victim of inability to practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.68).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.52).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation: Study as Archiving (Post-Temple Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic authority").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b').
narrative_ontology:cs_kernel_codification('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', fixed_text).
narrative_ontology:cs_authority_grounding('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', lineage).
narrative_ontology:cs_interpretation_layer_present('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b').
narrative_ontology:cs_reading_relation('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', foundational, binding_obligation_without_performability).
narrative_ontology:cs_axiom_status(binding_obligation_without_performability, holdable).
narrative_ontology:cs_axiom_grounding('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', binding_obligation_without_performability, deontological).
narrative_ontology:cs_axiom('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', foundational, study_preserves_not_fulfills).
narrative_ontology:cs_axiom_status(study_preserves_not_fulfills, holdable).
narrative_ontology:cs_axiom_grounding('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', study_preserves_not_fulfills, deontological).
narrative_ontology:cs_reference_frame('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', temple_destruction_binding_preservation).
narrative_ontology:cs_drift_state('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', contemporary_extended_exile, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a91c206-afe6-4b04-a2c0-fbb8ebad0d6b', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, textual_transmission_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, collective_israel).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, priestly_descendants).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, priestly_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces halakhic law, maintaining the binding status of the sacrifice obligation. Determines what counts as legitimate study and preservation. Benefits from centralized control over the meaning and application of unperformable law — the interpretation cannot be challenged by independent performance or empirical verification. Administers the constraint through yeshiva curricula, responsa literature, and institutional authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure, agenda_setter,
    institutional, generational, arbitrage, global).

% Bears the status of unfulfilled obligation. Required by halakhic authority to study sacrifice law as a mode of preserving knowledge, not as a path to actual compliance. Cannot exit this obligation without rejecting the halakhic framework that constitutes Jewish identity and practice. Experiences the constraint as perpetual non-compliance with binding status — the obligation is real, the fulfillment is impossible, the binding force is maintained by authority fiat.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, collective_israel, payer,
    organized, generational, identity_locked, global).

% Yeshivas, scribal academies, printing houses that preserve and transmit the text of sacrifice law. The archiving reading mandates their work as religiously obligatory — study of sacrifice law is not optional scholarship but binding Jewish practice. Benefit from the framing that makes their institutional mission religiously mandatory and integral to fulfilling collective obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, textual_transmission_institutions, beneficiary,
    institutional, generational, mobile, global).

% Descendants of the original priesthood (Kohanim), whose service in the Temple is preserved nominally in halakhic law but is substantively unfulfillable. The archiving reading preserves their role's honorific status and the requirement that they study sacrifice law, but blocks any actual practice. Benefit from honor and status in the framework; bear the cost of role without function.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, priestly_descendants, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, priestly_descendants, beneficiary).

% The theological narrative that knowledge preserved through study will enable restoration of sacrifice in the messianic age. This is not an agent but an abstract good the reading vindicated — the idea that the obligation's non-fulfillment is temporary and will be resolved in the future. The non-agent entry records this vindication without claiming restoration hope collects rents or bears costs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_restoration_hope, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, messianic_restoration_hope).

% Proponents of sibling readings (study_as_occupation, messianic_suspension) who interpret the obligation differently. Structurally excluded from authority to define how the obligation binds in the archiving framework — their interpretations are live in some communities but are not admitted as binding law within traditions that endorse the archiving reading. Would object that bindingness-without-fulfillment is unsustainable theology and unfair to Israel.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, alternative_halakhic_readings, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous chain of textual transmission and knowledge-preservation of sacrifice law across generations, ensuring that the procedures and conceptual framework of the sacrificial system are not lost. Coordinates scholarly engagement with divine law even when performance is impossible. Preserves rabbinic interpretive authority over the meaning and application of law.
% TRANSFER_FUNCTION: Moves the burden of unfulfilled obligation from the Temple period (when performance was possible but obstructed by Roman destruction) to every subsequent generation of Israel. Transfers the interpretive power to determine what counts as legitimate preservation and study to rabbinic authority structures. Transfers the work of knowledge-archival to yeshivas and transmission institutions.
% ABSENT_VOICES: Practitioners in diaspora communities who may not accept rabbinic authority's interpretation and who experience the obligation as merely historical or commemorative. Proponents of the sibling reading 'study_as_occupation' who argue study fulfills the obligation. Messianic-suspension adherents who argue the obligation is suspended, not binding. Rationalist/reformist movements that reject the binding status altogether. These voices are excluded from the core halakhic conversation that defines how the obligation binds within traditional Jewish law.
% DISAPPEARANCE_RATIONALE: If the archiving reading and its binding-without-fulfillment framework disappeared, Jewish practice would reorganize: either study of sacrifice law would cease to be obligatory (if adoption of the sibling study_as_occupation reading), or the obligation would be reframed as suspended (messianic_suspension), or it would be abandoned entirely (reformist exit). The halakhic curriculum would shift, rabbinic authority's interpretive power over this domain would weaken, and Israel's self-understanding of perpetual non-compliance would dissolve. The distribution of religious work and obligation would rearrange substantially.
% FOUNDING_PROBLEM: After the Roman destruction of the Second Temple (70 CE), the sacrificial system became impossible to perform. Yet the divine commandment to bring sacrifices remained written in Torah and was understood as eternally binding. The founding problem: how to maintain the binding character of divine law when material conditions make compliance impossible? How to preserve the knowledge and procedures for a distant future restoration without abandoning the obligation in the present?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authority attests that the founding problem (maintaining law's binding force after Temple destruction) justified the archiving framework. Historical-critical scholarship on Judaism after 70 CE corroborates that the destruction created an acute theological and legal crisis: the Torah commanded sacrifice, the Temple was gone, and Jewish legal tradition had to accommodate this gap. However, the founding problem's original urgency (the hope for rapid restoration and return to sacrifice) is now broadly acknowledged as dead — no contemporary Jewish movement genuinely expects Temple sacrifice restoration in any near or foreseeable future. Messianic restoration remains a theological claim in some traditions but not a practical expectation driving study practice. The archiving reading persists far beyond what its founding problem justifies, indicating mandatrophy resolution.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the constraint imposes asymmetric costs. This reading instantiates high extractiveness (0.68) because the obligation remains binding in perpetuity without being performable — Israel is locked into a normative relationship with an unexecutable command. The measurement series begins at 0.58 (early post-Temple period, when hope for rapid restoration was higher) and drifts to 0.68 by the modern period (as the restoration timeline extends indefinitely). Theater ratio climbs from 0.25 to 0.41 because over time, study of sacrifice law increasingly functions as commemorative ritual and identity-preservation rather than functional preparation for restoration — the justificatory narrative (archiving for future restoration) begins to perform knowledge-preservation itself, independent of restoration probability. Suppression is moderate (0.52) and stable because the constraint's enforcement depends on halakhic authority maintaining binding status, not on external coercion — the suppression is structural (authority fiat that the obligation remains binding) rather than physical, and internalized by practitioners who accept the framework.
 *
 * PERSPECTIVAL GAP:
 *   From rabbinic authority's seat, this reading is coherent preservation of halakhic knowledge under impossible material conditions — a rope that coordinates textual transmission across generations. From collective Israel's seat, it is an obligation imposed without any path to compliance, maintained in perpetuity by interpretive authority, with study functioning as a substitute for fulfillment rather than a path toward it — a snare disguised as preservation. The engine computes these divergent seats from the structural data: powerful institutional beneficiary (moderate power, arbitrage exit, can exit the obligation-framework by reinterpreting halakha) vs. organized/powerless victim (constrained exit, cannot reject halakhic authority without rejecting the identity-defining system). The perspectival gap is real and structural, not merely attitudinal.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority benefits from this reading: it centralizes interpretive power over an obligation that no one can independently verify or challenge (no one can perform sacrifice outside their authority). The framing gives rabbis institutional control over what counts as legitimate preservation and study. Collective Israel bears the cost: bound to an obligation they cannot fulfill, unable to exit the framework without rejecting the entire halakhic system. The institution of textual transmission (yeshivas, scribal transmission) benefits because the reading mandates their work as religiously mandatory. Priestly descendants occupy an ambiguous position — their nominal role is preserved in honor, but its substantive practice is blocked. From the reading's structural logic, rabbinic authority + transmission institutions are beneficiaries; collective Israel is the victim. The directionality derivation would place beneficiaries near d=0.0 (full subsidization by the constraint's logic) and Israel near d=1.0 (full target). No directionality override is needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is clear: how to maintain halakhic law's binding character when external conditions (destruction of Temple) make performance impossible. The archiving reading resolves this by decoupling bindingness from performability — the obligation remains binding, study preserves knowledge, restoration will eventually occur. But mandatrophy is present: the constraint persists far beyond what the founding problem justifies. If the goal was archival preservation, it could be achieved by scholarly documentation alone, without maintaining binding obligation on the collective. The binding status serves no purpose except to preserve rabbinic authority's interpretive power. The measurement series shows theater rising (0.25 → 0.41) even as restoration probability fell — a diagnostic signal that the constraint persists through performative justification rather than functional necessity. The reading does NOT resolve to pure rope (genuine coordination) because the study obligation is not collectively chosen; it resolves to tangled_rope (coordination of transmission + extraction of binding non-compliance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_fulfillment_boundary,
    'Is study-as-preservation functionally distinct from study-as-occupation, or do they describe the same activity with different theological justifications?',
    'Comparative textual analysis of Talmudic and post-Talmudic sources on whether study fulfills the obligation or merely preserves it; institutional practice in communities that explicitly designate study as archival vs. those that claim occupational status.',
    'If functionally identical, the two readings are rhetorical variants of the same constraint; if distinct, archiving-reading stands as a separate constraint with its own extraction profile. The boundary case is ambiguous in halakhic sources.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archiving_vs_fulfillment_boundary, conceptual, 'Whether study preserves knowledge (non-fulfilling) or occupies the obligation (fulfilling) — a functional vs. normative distinction.').

omega_variable(
    perpetual_non_compliance_status,
    'Does maintaining an unperformable, binding obligation constitute structural coercion on the collective (the entire post-Temple era as mandated non-compliance), or does it constitute a form of moral/textual preservation that requires no performance?',
    'Phenomenological study of how practitioners experience the obligation''s bindingness without performability; comparison with other unperformable divine commands in halakhic jurisprudence (e.g., sacrificial rites on festivals outside the Temple period).',
    'If bindingness without performability is experienced as coercive, extraction is high (victim set = Israel, unable to fulfill); if experienced as moral status without enforcement, extraction drops and the constraint moves toward rope. The suppression scalar reflects structural bindingness, not subjective experience, but the empirical question directly addresses whether suppression is structural or internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_non_compliance_status, empirical, 'Whether continuous binding status on an unperformable obligation constitutes structural suppression or normative status.').

omega_variable(
    kernel_reading_distinctiveness,
    'This reading (study-as-archiving) is one of three readings of the contested kernel ''temple_sacrifice_obligation''. Are the three readings genuinely distinct constraints with different ε values and victim structures, or are they interpretive layers over a single invariant obligation?',
    'Each reading is authored as a separate constraint story (separate files, linked via network.affects_constraints). The reading-specific ε value is fixed by THIS story''s structural analysis (archiving: non-fulfilling, binding, extractive). Sibling readings author their own ε under their own halakhic premises. Divergence between readings'' ε values indicates the kernel is genuinely contested — the readings instantiate different constraints.',
    'If readings are truly distinct constraints, the corpus carries three separate stories showing how the same halakhic kernel can be structured as different constraint types (tangled_rope here, different types in sibling readings). The network.affects_constraints links make the family relationship explicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinctiveness, conceptual, 'The ontological relationship between kernel readings and constraint individuation (ε-invariance principle applied to kernel contestation).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.25).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.3).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.39).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1900, 0.41).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.67).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 500, 0.49).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.51).
narrative_ontology:measurement(temp_su_t1900, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1900, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, resource_allocation).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'temple_sacrifice_obligation'. The kernel contest is halakhic: different Jewish interpretive traditions instantiate different structural understandings of how the sacrifice obligation binds (or does not bind) in the post-Temple period. Each reading is authored as a separate constraint story with its own ε, its own victim/beneficiary structure, its own claimed type. Network linkage makes the family relationship explicit. The archiving reading (this file) maintains binding status without performability, resulting in high extractiveness. Sibling readings differ: study_as_occupation reads study as fulfilling the obligation (likely lower extractiveness, rope type), and messianic_suspension reads the obligation as suspended (scaffold or mountain type, depending on whether suspension is temporary). The readings do not foreclose each other — they coexist as live halakhic positions held by different branches of Jewish tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
