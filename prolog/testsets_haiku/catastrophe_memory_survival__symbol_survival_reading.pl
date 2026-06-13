% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Rabbinic Ritual Authority and Identity Continuity (Symbol-Survival Reading)
 *   domain: religious/cultural/collective_memory
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'catastrophe_memory_survival' — the reading that positions ritual
 *   practice itself as the primary vehicle and constitutive mechanism of
 *   Jewish identity and continuity. The symbol-survival reading claims that
 *   survival IS the continuity of prescribed ritual form; that Jewish
 *   identity persists through participation in the ritual corpus according to
 *   rabbinic standard; that the catastrophe's lesson is that institutional
 *   ritual structures, not individual meaning-making or secular cultural
 *   work, must anchor collective identity. The constraint operates via
 *   institutional authority (rabbinic structures) enforcing conformity to
 *   prescribed symbolic form, with substantial extraction from practitioners
 *   who experience identity pressure while maintaining genuine participation.
 *   The reading competes against the competence_transmission reading
 *   (survival through embedded practical knowledge) and the hybrid_encoding
 *   reading (both registers together). This story models only the
 *   symbol-survival reading as a single, internally consistent constraint.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structures: institutional agenda-setter controlling liturgical standard and interpretive monopoly
 *   - traditional_observant_communities: organized beneficiaries whose lived experience aligns with the constraint's framing
 *   - secularized_diaspora_jews: moderate-power payers bearing cultural pressure and exclusion
 *   - non_hebrew_literate_practitioners: powerless payers trapped in interpretive dependence
 *   - assimilationist_secular_intellectuals: powerful excluded voices who would reframe survival
 *   - post_catastrophe_institutional_historians: analytical observers measuring whether the constraint's narrative matches empirical continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.72).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Rabbinic Ritual Authority and Identity Continuity (Symbol-Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious/cultural/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'd9b4c67b-a68f-4f4e-b94a-e80192aa71e4').
narrative_ontology:cs_kernel_codification('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', fixed_text).
narrative_ontology:cs_authority_grounding('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', lineage).
narrative_ontology:cs_interpretation_layer_present('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4').
narrative_ontology:cs_reading_relation('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', foundational, ritual_form_constitutive_of_identity).
narrative_ontology:cs_axiom_status(ritual_form_constitutive_of_identity, holdable).
narrative_ontology:cs_axiom_grounding('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', ritual_form_constitutive_of_identity, deontological).
narrative_ontology:cs_axiom('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', foundational, institutional_rabbinic_authority_required_for_legitimacy).
narrative_ontology:cs_axiom_status(institutional_rabbinic_authority_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', institutional_rabbinic_authority_required_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', catastrophe_survival_via_prescribed_ritual).
narrative_ontology:cs_drift_state('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', contemporary_secularization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d9b4c67b-a68f-4f4e-b94a-e80192aa71e4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority_structures).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, non_hebrew_literate_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, traditional_observant_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, identity_through_ritual_participation).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, continuity_as_survival_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and interprets the ritual corpus as the primary vehicle of Jewish identity and continuity. Sets the liturgical standard, determines which practices are essential to survival, controls the normative reading of symbolic meaning. Benefits from the constraint by preserving their interpretive monopoly: the framing that 'survival = ritual continuity' makes ritual expertise non-negotiable and outsources legitimacy questions to the rabbinic tradition itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority_structures, agenda_setter,
    institutional, civilizational, analytical, global).

% Experience ritual practice as the lived center of identity and community belonging. The constraint aligns their experienced continuity with rabbinic teaching: participation in prescribed ritual IS survival, is continuity, is identity. They are beneficiaries of a framework that validates their mode of practice and positions it as the non-negotiable baseline.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, traditional_observant_communities, beneficiary,
    organized, generational, identity_locked, local).

% Bear the cost of the constraint through several mechanisms: (1) cultural pressure to perform rituals despite secular conviction, generating cognitive dissonance and inauthenticity; (2) exclusion from full community standing if they construct identity through other registers (intellectual, artistic, political contribution); (3) isolation if they attempt to transmit Jewishness to children via non-ritual channels — the constraint renders these transmissions illegitimate as identity-carriers. They cannot exit without bearing the cost of perceived community abandonment.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews, payer,
    moderate, biographical, constrained, national).

% Depend entirely on rabbinic mediation to access the symbolic meaning the constraint says they must experience. Cannot verify or reinterpret the meaning the ritual claims to embody without Hebrew literacy and access to textual tradition. The constraint positions them as perpetual dependents on interpretive authority — their participation is genuine but their understanding is prescribed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, non_hebrew_literate_practitioners, payer,
    powerless, biographical, trapped, local).

% Are structurally excluded from the conversation about what Jewish survival requires. They argue that secular intellectual culture, political participation, and artistic creation are equally valid carriers of Jewish identity and continuity. They would reframe 'survival' to include transmission of ethical philosophy, historical consciousness, or cultural achievement outside the ritual register.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, assimilationist_secular_intellectuals, excluded,
    powerful, biographical, arbitrage, national).

% Examines whether ritual survival after catastrophe actually preserved Jewish identity, or whether it preserved institutional authority structures while actual identity-transmission occurred through secular cultural work. Can measure whether communities that maintained ritual fidelity also sustained cultural meaning, or whether the ritual became performative while meaning migrated elsewhere.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, post_catastrophe_institutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority_structures).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual practice solves the collective-action problem of boundary maintenance in diaspora: without a common symbolic vocabulary and regular participatory practice, dispersed communities lose the felt continuity of shared identity. The symbol-survival reading coordinates on a single answer: 'What keeps us Jewish?' — Answer: ritual participation in the prescribed form.
% TRANSFER_FUNCTION: Moves interpretive authority and identity-legitimacy from individual or family-level meaning-making to institutional rabbinic structures. Transfers authority to define what 'being Jewish' requires from practitioners to clergy. Moves the burden of authenticity from the participant's own conviction to conformity with prescribed form.
% ABSENT_VOICES: Secular Jewish intellectuals, artists, and political actors are structurally excluded. They would argue that Jewishness survived through literature, philosophy, political resistance, and cultural innovation — not primarily through ritual. Communities that achieved identity-continuity through other registers (academic achievement, artistic production, political engagement) are not heard in the conversation framing survival.
% DISAPPEARANCE_RATIONALE: Rabbinic authorities claim: if the ritual constraint vanished, Jewish identity would dissolve within two generations. Excluded voices claim: Jewish identity would reorganize around cultural memory, intellectual tradition, and ethical frameworks; it would NOT disappear. The historical record suggests both are partially correct.
% FOUNDING_PROBLEM: After the catastrophe and ongoing in diaspora, Jewish survival required mechanisms to maintain collective identity, memory, and boundary-norms across geographic dispersal and hostile assimilation pressure. The rabbinic reading: ritual form itself, performed according to prescribed standard, is the irreducible minimum that guarantees survival.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and traditional communities affirm that ritual continuity did preserve identity. Historians and secular Jewish intellectuals attest that survival occurred also through non-ritual transmission — political consciousness, literary production, memorial practice. The post-1945 empirical record shows identity survived through multiple channels; the constraint's monopoly claim (ritual = survival) was not disproven but was not the only answer.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers interpretive authority from practitioners to institutional structures; the identity-continuity it promises depends on compliance with prescribed form, not on individual conviction or alternative meaning-making. The extraction rises gradually over the interval (0.52→0.68) as secularization pressure increases and the constraint must work harder to maintain conformity — additional enforcement mechanisms emerge (exclusion from leadership, questioning of authenticity). Suppression is high (0.72) because the constraint actively suppresses alternative readings of what Jewish survival requires; secularized practitioners cannot validate their identity-transmission through non-ritual channels without being positioned as threats to continuity. Theater ratio rises sharply (0.35→0.58) and plateaus, indicating that enforcement mechanisms increasingly focus on maintaining the symbolic form itself rather than on the coordination function; ritual performance becomes validated as-performed, irrespective of participants' actual conviction or comprehension. Accessibility collapse is high (0.79) because once practitioners internalize the constraint's framing, alternatives (secular identity-transmission, cultural memory through literature, political engagement as identity-carrier) become cognitively unavailable — the constraint defines what counts as real survival. Resistance is low (0.41) because traditional communities genuinely experience identity continuity through ritual, making active resistance sparse; secularized practitioners resist through disengagement rather than confrontation, and the institutional capacity to suppress alternative frameworks is high.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority perspective, the constraint is pure coordination: 'We solved the survival problem by anchoring identity in a ritual practice that can survive any geography, any language barrier, any material deprivation — as long as the form persists, we survive.' From the secularized practitioner perspective, the same structure operates as extraction: 'I am pressured to perform rituals I do not experience as meaningful, excluded if I transmit identity through literature or ethics instead, trapped between conformity and perceived abandonment of my people.' From the post-catastrophe historian perspective, the constraint appears theatrically maintained: 'The rabbinic framing that ritual = survival was not tested; survival occurred through multiple channels, including secular ones the constraint renders invisible. The enforcement of symbolic conformity increased as the coordination function weakened.' The engine computes per-seat directionality from these structural asymmetries — the rabbinic seat sees subsidy (d~0.0), the secularized seat sees extraction (d~0.9), the historian sees the theater holding form-compliance via suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority benefits directly: control of the interpretive corpus, institutional monopoly on legitimacy, extracted compliance framed as voluntary identity-participation. Their directionality is low (toward beneficiary end) because the constraint subsidizes their authority. Traditional communities are near-symmetric (d~0.5): genuine coordination benefit (community belonging, clarity of identity-path) paired with subordination to institutional reading and identity-fusion costs. Secularized diaspora Jews are near the target end (d~0.85): they bear the cognitive dissonance cost (performing practices they experience as inauthentic), the exclusion cost (being positioned as threats to survival if they pursue alternative identity-transmission), and the isolation cost (losing community standing if they leave). Non-Hebrew-literate practitioners are at the target end (d~0.95): trapped in interpretive dependence, unable to verify the meaning they are told they must experience, unable to create parallel transmission without being delegitimized. Secular intellectuals are excluded (not in the participation structure at all), so directionality is undefined — they would compute as targets (d~1.0) if included, because the constraint actively suppresses their legitimacy and their voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is: 'How does Jewish identity and collective continuity survive catastrophe and diaspora?' This reading answers: 'Through ritual form, prescribed and institutional, performed by the community according to rabbinic standard.' The problem's current status is contested because empirical events since secularization show: (1) communities that maintained high ritual observance DID maintain institutional continuity and felt identity-continuity; (2) communities that abandoned ritual largely lost these forms of continuity; BUT (3) communities that abandoned ritual while maintaining cultural literacy, historical consciousness, and ethical philosophical frameworks DID preserve Jewish distinctiveness and transmitted it to subsequent generations. The constraint's answer was not disproven, but it was not the only answer — the monopoly claim (that survival requires ritual continuity) was empirically falsified. Yet the constraint persists and even intensifies (theater rising, suppression requirement climbing). This is the mandatrophy signature: the founding problem has fragmented into multiple viable answers, the rabbinic reading no longer has unchallenged authority, but the constraint's enforcement machinery strengthens because institutional actors depend on the monopoly framing for their legitimacy. The constraint has become self-maintaining (institutional authority depends on the claim that ritual is non-negotiable) rather than problem-solving (ritual no longer demonstrably solves the original survival question).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_cost_vs_survival_benefit,
    'For secularized practitioners, does the cognitive dissonance and inauthenticity of performing prescribed rituals count as a cost borne for the benefit of group survival, or as evidence that the constraint is extractive rather than coordinative?',
    'Qualitative interviews with secularized practitioners about whether they experience ritual performance as sacrifice-for-continuity or coerced-inauthenticity, measured against actual identity-transmission outcomes in their families. Does ritual performance by non-convinced participants actually improve identity-persistence in children, or does it transmit alienation?',
    'If authenticity cost is experienced as justified sacrifice, the constraint is tangled_rope (real coordination + bearable asymmetry). If experienced as coercion generating alienation, the constraint reclassifies toward snare (pure extraction hiding behind coordination framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_cost_vs_survival_benefit, empirical, 'Whether suppressed authenticity represents cost-sharing for group benefit or evidence of pure extraction.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.72) primarily structural (external institutional enforcement, exclusion from leadership, community pressure) or internalized (practitioners have absorbed the reading that ritual = survival and suppress their own alternative identity-frameworks)?',
    'Post-exit suppression trajectory: if practitioners who abandon ritual observance also lose the sensation of internal pressure to conform, suppression was primarily internalized; if external exclusion or institutional pressure persists after exit, suppression is primarily structural. Comparative case: communities that dissolved institutional enforcement while maintaining ritual (e.g., non-denominational Jewish communities with voluntary observance) should show dramatically different suppression profiles if mechanisms differ.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit, making reclassification harder and the identity-lock deeper. If primarily structural, exit is more feasible once enforcement machinery is dismantled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Internalized versus structural suppression mechanism in identity-locked constraint.').

omega_variable(
    empirical_survival_continuity_mismatch,
    'Does the historical record show that communities maintaining high ritual observance also maintained Jewish identity-continuity better than communities abandoning ritual? Or did identity-continuity persist through multiple channels (secular cultural work, political consciousness, literary tradition) regardless of ritual observance?',
    'Comparative historical analysis: post-1945 communities with high institutional ritual observance vs. secular Jewish intellectual/political/artistic communities; measure identity-persistence in third and fourth generations, Jewish identification rates, cultural production, community participation, and whether descendants identify as Jewish.',
    'If ritual-maintaining communities show significantly higher identity-persistence AND secular communities show significant identity-attrition, the constraint''s framing (ritual = survival) is empirically validated and the extraction becomes more defensible as problem-solving. If secular communities maintain comparable or different-form identity-persistence, the constraint''s monopoly claim is falsified and the extraction becomes pure institutional power maintenance. This determines whether theater_ratio rise indicates genuine enforcement degradation or performative maintenance of a claim known to be false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_survival_continuity_mismatch, empirical, 'Whether the founding problem (survival through catastrophe) is actually solved by the constraint''s prescribed method.').

omega_variable(
    kernel_reading_contest_ground,
    'What is the most basic structural disagreement between the symbol-survival reading and its sibling readings? Is it about what survives (group boundary vs. practical knowledge vs. both), about the mechanism (symbolic form vs. embedded competence vs. integrated), or about who adjudicates legitimacy (institutional authority vs. practitioner competence vs. distributed)?',
    'Qualitative analysis of competing arguments in rabbinic literature, secular Jewish historiography, and post-Holocaust memorial practice. The disagreement that would survive the other readings being disproven is the ground of the reading contest.',
    'Clarifies whether the three readings are empirically contestable (competing hypotheses about what actually survives) or foundationally contested (different definitions of ''survival'' itself). If empirical, the container-constraint (which reading is true?) is classified by evidentiary adequacy; if foundational, the readings are incommensurable and the constraint is properly the exercise of institutional authority to impose one definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_ground, conceptual, 'The deepest ground of disagreement between symbol-survival and its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(cata_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the catastrophe_memory_survival kernel family. The kernel is the question 'What practices/knowledge/structures must survive the catastrophe for Jewish continuity to persist?' Three structurally distinct readings decompose this: (1) symbol_survival_reading (THIS STORY) — continuity = ritual form, institutional authority preserves it, high extraction from non-authentic practitioners; (2) competence_transmission_reading — continuity = embedded practical knowledge, distributed transmission, lower extraction; (3) hybrid_encoding_reading — continuity requires both, inseparable, moderate extraction. Each reading has its own constraint_id, its own ε, its own beneficiary/victim structure, and its own type. The network links them because they are all answers to the same kernel question and compete for institutional legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
