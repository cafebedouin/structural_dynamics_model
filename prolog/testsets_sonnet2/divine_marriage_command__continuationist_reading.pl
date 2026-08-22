% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuationist Reading of the Divine Marriage Command (Polygamy as Unrescinded Doctrine)
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   This story instantiates the continuationist reading of the divine
 *   marriage command kernel: the position, held by fundamentalist splinter
 *   groups descended from the 19th-century mainstream church, that the
 *   original revelation commanding plural marriage was never doctrinally
 *   rescinded — the 1890 Manifesto was a prudential, externally-coerced
 *   suspension issued under threat of federal prosecution and church
 *   disincorporation, not a new revelation superseding the old one. Under
 *   this reading, current plural marriage practice within splinter
 *   communities is theologically continuous and legitimate, and federal/state
 *   law prohibiting polygamy is treated as an external constraint to be
 *   navigated, not as evidence the command was internally revised. This is a
 *   distinct constraint from the substitutionist reading (which holds
 *   monogamy is now doctrinally required by genuine new revelation) and from
 *   the coercion_visibility_reading (which treats the Manifesto as an
 *   acknowledged survival response whose legitimacy derives precisely from
 *   institutional necessity, without adjudicating whether the underlying
 *   command persists). The ε authored here (0.62) reflects extraction as it
 *   operates within the communities that hold this reading — concentrated
 *   status and reproductive/labor benefit flowing to patriarchs and the
 *   hierarchy, at cost to the women, minors, and dissenters positioned
 *   beneath them in the marriage-assignment order — assessed by this
 *   reading's own lights, not by the more moderate ε that would attach to a
 *   merely private or consensual continuationist practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.62).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuationist Reading of the Divine Marriage Command (Polygamy as Unrescinded Doctrine)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, 'f7342aac-da87-46ef-95a3-4403de67c716').
narrative_ontology:cs_kernel_codification('f7342aac-da87-46ef-95a3-4403de67c716', fixed_text).
narrative_ontology:cs_authority_grounding('f7342aac-da87-46ef-95a3-4403de67c716', lineage).
narrative_ontology:cs_interpretation_layer_present('f7342aac-da87-46ef-95a3-4403de67c716').
narrative_ontology:cs_reading_relation('f7342aac-da87-46ef-95a3-4403de67c716', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('f7342aac-da87-46ef-95a3-4403de67c716', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('f7342aac-da87-46ef-95a3-4403de67c716', foundational, original_revelation_remains_binding_absent_new_revelation).
narrative_ontology:cs_axiom_status(original_revelation_remains_binding_absent_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('f7342aac-da87-46ef-95a3-4403de67c716', original_revelation_remains_binding_absent_new_revelation, theological).
narrative_ontology:cs_axiom('f7342aac-da87-46ef-95a3-4403de67c716', foundational, prudential_policy_statements_cannot_rescind_doctrinal_command).
narrative_ontology:cs_axiom_status(prudential_policy_statements_cannot_rescind_doctrinal_command, holdable).
narrative_ontology:cs_axiom_grounding('f7342aac-da87-46ef-95a3-4403de67c716', prudential_policy_statements_cannot_rescind_doctrinal_command, theological).
narrative_ontology:cs_reference_frame('f7342aac-da87-46ef-95a3-4403de67c716', original_revelation_unbroken_lineage).
narrative_ontology:cs_drift_state('f7342aac-da87-46ef-95a3-4403de67c716', post_second_manifesto_and_federal_prosecution_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f7342aac-da87-46ef-95a3-4403de67c716', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_patriarchs).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, splinter_church_hierarchy).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_wives_in_splinter_communities).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, minor_brides).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, excommunicated_dissenters).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, original_revelation_unbroken).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, manifesto_as_tactical_suspension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governs closed fundamentalist communities descended from the mainstream church's 19th-century plural-marriage practice. Administers marriage assignment, membership, and excommunication. Asserts unbroken continuity with the original revelation and treats the 1890 Manifesto as a political concession to federal prosecution, not a theological reversal. Controls property, employment, and social standing within the community, which gives it durable leverage even as it faces external legal jeopardy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, splinter_church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, splinter_church_hierarchy, beneficiary).

% Adult men who hold multiple wives under the continuationist reading, receiving social status, household labor, and reproductive capacity assignment through the hierarchy's marriage-placement authority. Their standing depends entirely on the doctrine remaining live; if the community reclassified polygamy as historically superseded, their current marriages and status would be delegitimized within the only social order they recognize.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_patriarchs, beneficiary,
    powerful, generational, identity_locked, local).

% Women placed into marriages, often without independent legal standing recognized outside the community, whose exit is blocked by economic dependency, lack of outside education or documentation, threat of losing custody of children, and the doctrine's own teaching that leaving invites damnation. They bear the direct costs of the marriage-command's continued doctrinal force.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_wives_in_splinter_communities, payer,
    powerless, biographical, trapped, local).

% Underage girls placed into marriage in some splinter communities under the reading that the original command was never rescinded, only tactically suspended. They have no independent capacity to consent, no access to outside authorities who are not viewed as hostile, and no exit route recognized by the community that placed them.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, minor_brides, payer,
    powerless, immediate, trapped, local).

% Former members — often young men denied wives under the community's marriage-assignment hierarchy ('Lost Boys'), or women and families who question the continuationist reading — who are expelled, lose contact with remaining family, and lose housing and employment tied to the community. Their departure is possible but comes at severe relational and material cost, which the doctrine's continued force makes structurally necessary to enforce.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, excommunicated_dissenters, payer,
    moderate, biographical, constrained, local).

% Prosecute bigamy, child marriage, and welfare fraud where documented, but operate largely outside the community's internal theological framework and are treated by the hierarchy as an external coercive force whose demands do not touch doctrine. Their enforcement shapes the community's public posture without altering its internal reading of the marriage command.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_and_state_authorities, excluded,
    institutional, generational, analytical, national).

% Formally disavows the continuationist reading and treats the Manifesto as binding doctrinal revision (the substitutionist position), actively excommunicating members found practicing plural marriage. From inside the mainstream institution, the continuationist reading is heresy; it has no voice within the splinter hierarchy's internal deliberations and is invoked only as a foil.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_leadership, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, mainstream_church_leadership, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, splinter_church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internally coherent doctrinal identity and marriage-and-inheritance ordering system for communities that reject the mainstream church's 1890 policy shift, allowing continuity of practice, lineage claims, and communal cohesion across generations of legal pressure.
% TRANSFER_FUNCTION: Moves marital assignment authority, reproductive and household labor, and social status from women and junior men to senior patriarchs and the hierarchy that administers placement; moves legitimacy and continuity claims from the mainstream institution to the splinter hierarchy.
% ABSENT_VOICES: Plural wives and minor brides who might reject the marriage-command reading are functionally absent from doctrinal deliberation — the hierarchy that authors and enforces the reading is composed of the men who benefit from it. Federal and state authorities are structurally external and are not treated as a source of theological input at all, only coercive pressure to be weathered.
% DISAPPEARANCE_RATIONALE: If the continuationist reading vanished — if the community accepted the Manifesto as binding doctrinal rescission — the entire marriage-assignment and status hierarchy built on plural marriage would lose its theological warrant. Patriarchs' multiple marriages would lose religious legitimacy, wives currently trapped by doctrinal teaching about the sin of leaving would gain a recognized exit narrative, and the hierarchy's core claim to unbroken continuity with the original revelation would collapse, likely fragmenting the community's authority structure entirely.
% FOUNDING_PROBLEM: The reading was built to resolve the tension between an 1830s revelation commanding plural marriage (as the founders understood it) and an 1890 Manifesto issued under acute federal prosecution and disincorporation threat — the community needed a way to comply publicly with federal law while preserving the theological claim that the original command remained divinely valid and binding on the faithful.
% FOUNDING_PROBLEM_CORROBORATION: The splinter hierarchy and its patriarchs attest the founding problem is still live — that the original command was never rescinded, only suspended under duress, and remains binding. The mainstream church leadership, most contemporary legal scholars, and state prosecutors attest the opposite: that the Manifesto and subsequent 1904 Second Manifesto constitute genuine doctrinal closure, and that the continuationist reading is a post-hoc justification maintained by parties who materially benefit from continued plural marriage. No corroboration for the continuationist reading's 'unbroken command' claim comes from outside the splinter communities themselves or their direct doctrinal descendants.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.35 to 0.62) as splinter communities consolidated marriage-assignment authority into fewer, more powerful patriarchal lines (the Short Creek/FLDS pattern) and as external legal pressure paradoxically hardened internal doctrinal enforcement rather than dissolving it. Suppression is high (0.71) because maintaining the reading against both external prosecution and internal dissent requires active enforcement: excommunication of dissenters, isolation of members from outside information, and control over exit costs (housing, custody, social network). Theater ratio is comparatively low (0.28) — this is not primarily a performative constraint; the doctrinal claim organizes real marriage assignment, property, and household structure, though a rising theater component reflects increasing public-facing 'religious liberty' framing distinct from internal practice.
 *
 * DIRECTIONALITY LOGIC:
 *   The splinter hierarchy and patriarchs sit near the beneficiary end: they set the reading, administer its consequences, and receive status and marital/reproductive assignment through it. Plural wives, and especially minor brides, sit at the target end: trapped exit options, no independent legal or social standing recognized within the community, and direct bearing of the marriage command's costs. Excommunicated dissenters occupy an intermediate position — constrained rather than fully trapped exit, but at severe relational cost, which is precisely the enforcement mechanism keeping the reading operative for those who remain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling an original command with acute 1890s federal coercion) is contested rather than dead: the splinter hierarchy insists the underlying command is still live and unaddressed by any legitimate revision, while virtually every corroborating source outside the benefiting hierarchy — the mainstream church, courts, most historians — holds the founding problem was resolved by the Manifesto itself. This mismatch (status: contested, but disappearance_verdict: world_rearranges) is exactly the signature the R5 genealogy check is built to catch: an arrangement whose self-declared continuity claim is corroborated almost entirely by its own beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_rescission_or_suspension,
    'Did the 1890 Manifesto constitute genuine doctrinal rescission of the plural marriage command, or merely a tactical suspension issued under duress that leaves the underlying command theologically intact?',
    'There is no external empirical test for this — it is an internal theological/doctrinal question resolved differently by different faith communities based on their own hermeneutic commitments (continuity of revelation vs. capacity for later revelation to supersede earlier command). Historical record of the coercive context (Edmunds-Tucker Act, church disincorporation threat, mass arrests) is not itself dispositive of the theological question, though it is dispositive of the coercion_visibility_reading''s narrower claim.',
    'If treated as genuine rescission, the continuationist reading dissolves into simple doctrinal error or bad-faith persistence, sharpening its classification toward pure extraction (snare) for those harmed by continued practice. If treated as valid suspension-not-rescission, some genuine coordination/continuity function persists for the reading''s holders, supporting the tangled_rope classification authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manifesto_rescission_or_suspension, conceptual, 'Whether the Manifesto is doctrinal rescission or mere prudential suspension — the central theological fork this reading takes one side of.').

omega_variable(
    beneficiary_capture_of_doctrinal_interpretation,
    'To what extent is the continuationist reading maintained because it is theologically correct by the tradition''s own interpretive standards, versus because it is materially and socially advantageous to the specific men who administer and benefit from it?',
    'Compare doctrinal interpretation patterns across splinter groups with different marriage-assignment structures (e.g., groups with more equal distribution of wives vs. concentrated patriarchal control) and see whether interpretive rigor correlates with material stake, or examine whether any splinter faction holding the continuationist reading has concluded against its own patriarchs'' material interest.',
    'High correlation between material benefit and doctrinal position would support classifying this as extraction wearing doctrinal cover (closer to snare); low correlation would support a more genuine coordination/identity-continuity reading (closer to rope or tangled_rope as authored).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_doctrinal_interpretation, empirical, 'Whether doctrinal conclusions track material self-interest of the interpreting hierarchy.').

omega_variable(
    consent_capacity_of_plural_wives,
    'Is the suppression measured here structural (economic dependency, legal non-recognition, custody threat) or partly internalized (belief instilled from childhood that leaving is itself sin, absent any external barrier)?',
    'Post-exit trajectory: track women who leave splinter communities and observe whether reported distress/suppression persists after external barriers (housing, custody, legal status) are resolved — persistence would indicate an internalized component requiring separate remediation.',
    'If largely internalized, effective suppression for wives and minor brides is higher than the structural exit-options atom alone would suggest, and departure support programs must address belief-level suppression, not just material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_capacity_of_plural_wives, empirical, 'Structural vs. internalized suppression mechanism for plural wives within splinter communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__continuationist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(divi_tr_t20, observed).
narrative_ontology:measurement(divi_tr_t45, divine_marriage_command__continuationist_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement_basis(divi_tr_t45, observed).
narrative_ontology:measurement(divi_tr_t70, divine_marriage_command__continuationist_reading, theater_ratio, 70, 0.23).
narrative_ontology:measurement_basis(divi_tr_t70, observed).
narrative_ontology:measurement(divi_tr_t100, divine_marriage_command__continuationist_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement_basis(divi_tr_t100, observed).
narrative_ontology:measurement(divi_tr_t130, divine_marriage_command__continuationist_reading, theater_ratio, 130, 0.28).
narrative_ontology:measurement_basis(divi_tr_t130, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__continuationist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(divi_be_t20, observed).
narrative_ontology:measurement(divi_be_t45, divine_marriage_command__continuationist_reading, base_extractiveness, 45, 0.5).
narrative_ontology:measurement_basis(divi_be_t45, observed).
narrative_ontology:measurement(divi_be_t70, divine_marriage_command__continuationist_reading, base_extractiveness, 70, 0.55).
narrative_ontology:measurement_basis(divi_be_t70, observed).
narrative_ontology:measurement(divi_be_t100, divine_marriage_command__continuationist_reading, base_extractiveness, 100, 0.6).
narrative_ontology:measurement_basis(divi_be_t100, observed).
narrative_ontology:measurement(divi_be_t130, divine_marriage_command__continuationist_reading, base_extractiveness, 130, 0.62).
narrative_ontology:measurement_basis(divi_be_t130, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__continuationist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__continuationist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(divi_su_t20, observed).
narrative_ontology:measurement(divi_su_t45, divine_marriage_command__continuationist_reading, suppression_requirement, 45, 0.63).
narrative_ontology:measurement_basis(divi_su_t45, observed).
narrative_ontology:measurement(divi_su_t70, divine_marriage_command__continuationist_reading, suppression_requirement, 70, 0.66).
narrative_ontology:measurement_basis(divi_su_t70, observed).
narrative_ontology:measurement(divi_su_t100, divine_marriage_command__continuationist_reading, suppression_requirement, 100, 0.69).
narrative_ontology:measurement_basis(divi_su_t100, observed).
narrative_ontology:measurement(divi_su_t130, divine_marriage_command__continuationist_reading, suppression_requirement, 130, 0.71).
narrative_ontology:measurement_basis(divi_su_t130, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family reading the same divine_marriage_command kernel: continuationist_reading (this story — command unrescinded, Manifesto as tactical suspension), substitutionist_reading (Manifesto as genuine superseding revelation, monogamy now required), and coercion_visibility_reading (Manifesto as acknowledged coercion-response, legitimacy from survival necessity, agnostic on underlying command status). Each reading carries a distinct beneficiary/victim structure and a distinct ε: the continuationist reading is authored with the highest extraction among the three because it is the reading under which current, ongoing plural marriage practice retains active theological warrant and is administered by a hierarchy with concentrated material interest in that warrant persisting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
