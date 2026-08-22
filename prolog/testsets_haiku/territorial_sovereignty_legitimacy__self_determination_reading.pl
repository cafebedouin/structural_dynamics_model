% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Self-Determination Reading
 *   domain: political/international
 *
 * SUMMARY:
 *   This constraint story instantiates one reading of the contested kernel of
 *   territorial sovereignty legitimacy in Israel/Palestine: the
 *   self-determination reading, which grounds sovereignty legitimacy in the
 *   self-determination right of the Arab population constituting demographic
 *   majority and claiming continuous residence in the territory during the
 *   modern period (19th-20th centuries). Under this reading, the 1948
 *   partition is framed as an external imposition against the will of the
 *   resident majority; the Israeli state is characterized as a colonial
 *   project; and Palestinian displacement and occupation are structural
 *   violations of self-determination rights. The claim and metrics are
 *   authored independently: the constraint is claimed as tangled_rope because
 *   the reading vindicates a genuine coordination function
 *   (self-determination principle) while enabling substantial extraction
 *   (territorial dispossession and ongoing occupation). The authored metrics
 *   describe a highly extractive, actively enforced arrangement whose
 *   persistence depends on military suppression and international legitimacy
 *   contestation.
 *
 * KEY AGENTS:
 *   - Palestinian population in territory: target of extraction; bears costs of partition and occupation; demographic majority under this reading; trapped exit.
 *   - Palestinian diaspora: dispersed by partition; claims right of return; bears ongoing displacement costs; moderate constrained exit.
 *   - Palestinian National Authority: agenda-setter; administers and enforces the self-determination claim; organized power; regionally constrained.
 *   - Arab regional states: beneficiaries by solidarity; powerful; mobile exit; validate the reading diplomatically.
 *   - Israeli state: excluded from legitimacy framework; powerful; trapped exit; operates under opposed reading.
 *   - International community (UN, states, NGOs): observers; institutional power; split enforcement (GA affirmation, Security Council blockage).
 *   - Western powers: observers; institutional power; historically positioned as imposers of partition; mobile exit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.79).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy: Self-Determination Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political/international").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'b4422c8e-dcb3-492e-a479-10c7079aa1ce').
narrative_ontology:cs_kernel_codification('b4422c8e-dcb3-492e-a479-10c7079aa1ce', formalized).
narrative_ontology:cs_authority_grounding('b4422c8e-dcb3-492e-a479-10c7079aa1ce', extraction).
narrative_ontology:cs_interpretation_layer_present('b4422c8e-dcb3-492e-a479-10c7079aa1ce').
narrative_ontology:cs_reading_relation('b4422c8e-dcb3-492e-a479-10c7079aa1ce', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('b4422c8e-dcb3-492e-a479-10c7079aa1ce', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('b4422c8e-dcb3-492e-a479-10c7079aa1ce', foundational, self_determination_right_primary_legitimacy).
narrative_ontology:cs_axiom_status(self_determination_right_primary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b4422c8e-dcb3-492e-a479-10c7079aa1ce', self_determination_right_primary_legitimacy, deontological).
narrative_ontology:cs_axiom('b4422c8e-dcb3-492e-a479-10c7079aa1ce', foundational, demographic_majority_continuous_residence_baseline).
narrative_ontology:cs_axiom_status(demographic_majority_continuous_residence_baseline, holdable).
narrative_ontology:cs_axiom_grounding('b4422c8e-dcb3-492e-a479-10c7079aa1ce', demographic_majority_continuous_residence_baseline, empirically_contingent).
narrative_ontology:cs_reference_frame('b4422c8e-dcb3-492e-a479-10c7079aa1ce', arab_demographic_self_determination_pre_partition).
narrative_ontology:cs_drift_state('b4422c8e-dcb3-492e-a479-10c7079aa1ce', contemporary_occupation_settlement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b4422c8e-dcb3-492e-a479-10c7079aa1ce', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_authority).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_regional_states).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_population_in_territory).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, un_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, anti_colonial_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the direct costs of partition and territorial displacement through restriction of movement, limited self-governance, and exclusion from decision-making about territory they claim continuous residence in. Under this reading, they are the rightful demographic majority whose self-determination right is systematically denied. Exit is constrained by military occupation, checkpoint systems, and legal restrictions on settlement and property rights.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_population_in_territory, payer,
    powerless, generational, trapped, local).

% Displaced by the 1948 partition and subsequent conflicts, they claim the right of return under the self-determination reading as restoration of their original territorial stake. They are exiled from the territory they assert they belong in, bearing psychological, economic, and political costs of diaspora status. Limited ability to return or claim property in the territory.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora, payer,
    moderate, generational, constrained, global).

% Represents and administers the Palestinian claim to self-determination. Attempts to enforce the reading through diplomatic channels, international legal arguments, and institutional assertion. Benefits from legitimacy granted by this framing but remains constrained in actual territorial control and enforcement capacity by superior military force and international recognition asymmetry.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_authority, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_authority, beneficiary).

% Frame the Palestinian claim as an Arab cause and validate the self-determination reading through diplomatic recognition and regional solidarity. Benefit from the reading's framing as it supports their own narratives about resistance to colonial partition and Western intervention. Can adjust their level of active support based on shifting strategic interests and international pressure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_regional_states, beneficiary,
    powerful, generational, mobile, regional).

% Operates under a fundamentally opposed reading (covenant_continuity or existential_matrix). Under the self-determination reading, Israel is framed as a colonial project imposed on territory whose rightful inhabitants are the Arab demographic majority. Israel is structurally excluded from the legitimacy framework this reading constructs—the reading's core logic denies the legitimacy of Israeli sovereignty in the territory. Would argue for alternative temporal baselines and legitimacy sources.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, excluded,
    powerful, generational, trapped, regional).

% UN institutions, NGO networks, and state actors observe and selectively enforce or validate this reading. The United Nations General Assembly has repeatedly passed resolutions affirming Palestinian self-determination rights; however, Security Council enforcement is blocked by permanent member vetoes. International legitimacy oscillates based on political coalitions and media framing rather than consistent juridical commitment.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_community, observer,
    institutional, generational, analytical, global).

% Historically positioned as external powers that imposed partition (British Mandate, UN Partition Plan, post-WWII geopolitics). The self-determination reading frames Western intervention as colonial imposition against the will of the resident Arab majority. Western states maintain formal commitment to self-determination doctrine while providing military and economic support to Israel, creating a structural tension the reading diagnoses as colonial hypocrisy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, western_powers, observer,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_authority).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a principle by which territorial sovereignty claims can be adjudicated: the self-determination right of a population with demographic majority and continuous historical residence in a territory. Theoretically solves the problem of how to allocate contested territorial claims fairly by centering resident population will rather than external power allocation or ancient historical claims.
% TRANSFER_FUNCTION: Moves legitimacy and territorial control from external powers and competing claimants toward the Palestinian population asserting demographic majority and continuous residence. Transfers recognition from the 1948 partition outcome (imposed externally) back to a pre-partition baseline of Arab demographic control. In effect, the reading asserts that occupation and Jewish immigration constitute extractive transfers FROM the Palestinian majority TO an imported Jewish minority.
% ABSENT_VOICES: Israeli Jews under the self-determination reading are rendered as external colonizers rather than indigenous claimants—their voice within this framework is systematically excluded because the reading's core logic denies their legitimacy as native inhabitants. Diaspora Jews with ancestry in the territory and historical trauma from persecution are present but not centered; their security concerns are treated as secondary to Palestinian self-determination. Secular Israeli settlers and Palestinian Christians both have stakes but are rendered secondary to the ethno-national framing.
% DISAPPEARANCE_RATIONALE: If the self-determination reading were formally abandoned, the territorial arrangement would shift: Palestinian claims to majority rule would lose their strongest juridical ground, right-of-return arguments would weaken, and the occupation would lose its 'illegitimate partition' framing. The balance of legitimacy would depend on alternative readings (covenant continuity, existential necessity) which would justify different territorial arrangements. The international law framework governing the conflict would reorganize around whichever reading gained ascendance.
% FOUNDING_PROBLEM: The partition of Palestine in 1948, authorized by external Western powers (UN Partition Plan), against the expressed opposition of the Arab majority population living in the territory. The reading traces the problem to the imposition of a Jewish state on territory where Arabs held demographic majority and centuries of continuous residence.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian national movements, Arab states, and the UN General Assembly (through repeated resolutions on Palestinian self-determination and the right of return) attest the founding problem is live. The International Court of Justice's 2004 advisory opinion on the Israeli separation barrier and multiple UN human rights bodies characterize the situation as a violation of self-determination rights. However, Israeli scholarship and Western governments contest this framing, arguing the founding problem (security of Jewish populations and existence of a Jewish state) takes precedence and was solved in 1948. The corroboration is strong from the reading's own constituencies but contested by excluded and opposed seats.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at endpoint) and rising because the reading diagnoses the territorial arrangement as inherently extractive: a partition imposed by external powers against the will of the resident Arab majority, followed by ongoing occupation, settlement expansion, and restrictions on Palestinian political autonomy and refugee return. This is not a dispute over marginal distribution but over the legitimacy of the foundational territorial arrangement itself. Suppression is high (0.79) because the reading's persistence as a live international claim depends on sustained resistance from Palestinian movements and Arab states against the enforcement machinery of Israeli military occupation and Western diplomatic blockage. Theater ratio is moderate-low (0.41) because the constraint carries both real coordination function (the self-determination principle is genuinely applied, internationally endorsed in UN resolutions) and real extractive operation (dispossession, occupation, settlement); the performative component is the gap between affirmed self-determination doctrine and selective enforcement against Palestinian claims. The measurement trajectory shows modest escalation: extractiveness and suppression both rise slightly over the interval, reflecting intensifying settlement expansion and hardening occupation enforcement, while theater remains stable—the gap between doctrine and practice persists without widening. Accessibility collapse is moderate (0.68): alternatives to the self-determination reading exist and remain live, so the reading has not totally collapsed other interpretive possibilities, but the modern principle of self-determination has become sufficiently entrenched in international law that rejecting it requires explicit ideological opposition rather than mere alternative framing.
 *
 * PERSPECTIVAL GAP:
 *   The Palestinian seats (population in territory, diaspora, PNA) experience this constraint as foundational illegitimacy and dispossession—a tangled rope where the coordination function (legitimate self-determination) is inseparable from the extraction mechanism (occupation and denial of that same right). From their position, the engine should compute high extraction and high necessity for enforcement to suppress resistance. The Israeli seat, operating under an opposed reading, experiences the same constraint as an illegitimate international delegitimization of Jewish sovereignty and security claims; from that position, the self-determination reading appears as a snare designed to undermine Israeli legitimacy. The Arab regional states benefit from the reading's existence (validates their position against Israel) while bearing some costs (military investment in the conflict, strategic limitation); they sit nearer the beneficiary end. The international community formally affirms the reading (UN General Assembly resolutions on self-determination and right of return) while selectively enforcing it (Security Council vetoes, uneven pressure on implementation), creating a structural contradiction the reading itself diagnoses as colonial hypocrisy. The engine's per-seat computation should surface these gaps: same constraint, radically different effective extraction and type across the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is high (near 1.0) for Palestinian powerless population and diaspora: they are the stated beneficiaries of the self-determination principle but structurally denied the substance of that right; the constraint's persistence requires active suppression of their claims and exit options. Directionality is moderate (0.4-0.6) for Palestinian National Authority: they benefit from the reading's legitimacy while remaining constrained in enforcement capacity; they set the agenda within Palestinian politics but not within the territorial arrangement itself. Directionality is low (0.1-0.3) for Arab regional states: they benefit from alignment with the reading while maintaining mobile exit (can adjust support based on strategic interest). Directionality is negative (approaching 0.0) for Israeli state: from the self-determination reading's perspective, Israel is the structural target whose legitimacy and territorial control the reading calls into question; Israeli directionality is fundamentally opposed. The international community's directionality is split by institutional position: UN General Assembly observers cluster near beneficiary (affirm the reading); Security Council permanent members are captured by strategic alignment with Israel and sit nearer neutral (0.5) in effective directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The self-determination reading presents a mandatrophy candidate: the founding problem (partition imposed against majority will) has not been solved; the arrangement persists because its enforcement capacity exceeds the political will to change it, not because the problem has been resolved or because participants derive sufficient benefit to justify sustained investment. The reading diagnoses the current state as structurally unstable—high extraction, high suppression, high resistance, with no path to legitimacy acceptance from the target seat. This is a tangled_rope (not a pure snare) because the self-determination principle does coordinate a genuine function (how to allocate territorial claims fairly among populations with conflicting interests), but the extraction component (dispossession, occupation) overwhelms the coordination component in actual operation. The engine's classification should flag the tension: tangled_rope at the beneficiary/international seats, snare or unstable tangled_rope at the target seats, based on directionality and exit constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_baseline_contestation,
    'Is the ''modern period'' (19th-20th centuries) the relevant temporal baseline for assessing continuous residence and demographic legitimacy, or do older periods (medieval, ancient) count equally?',
    'Philosophical and juridical analysis of what makes a temporal baseline legitimate for legitimacy claims. Does modern international law''s emergence displace older claims, or does it operate alongside them? Comparison with other self-determination cases where temporal baselines are contested.',
    'If only the modern period counts, Palestinian demographic majority is clear and the reading''s foundation is solid. If earlier periods (e.g., medieval Jewish presence, ancient Israelite kingdoms) count equally, the demographic and historical baseline becomes contested, undermining the reading''s exclusivity. This is fundamentally a question about what historical depth legitimacy requires—whether it is measuring continuous presence from the modern period backward or from ancient times forward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_baseline_contestation, conceptual, 'Dispute over the temporal scope of continuous residence claims.').

omega_variable(
    definition_of_continuous_residence,
    'What constitutes ''continuous residence'' given migration, dispersion, and re-immigration across the modern period? Does Jewish immigration into the territory during the modern period count as re-establishing presence (continuity) or as external settlement (interruption)?',
    'Demographic and historical analysis of population flows, settlement patterns, and migration. Genealogical research establishing family continuity vs. settlement history. Competing historical narratives from Palestinian and Israeli scholarship.',
    'If Jewish immigration is reframed as continuation of ancient presence rather than new settlement, the reading''s claim that Arabs hold sole continuous residence dissolves. This would undermine the exclusive legitimacy claim and require a coexistence or power-sharing framing rather than majority-rule self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_continuous_residence, empirical, 'Whether Jewish modern-period immigration interrupts or continues ancient presence claims.').

omega_variable(
    reading_kernel_relationship,
    'Is the self-determination reading a coherent instantiation of a single kernel (territorial_sovereignty_legitimacy), or does it beg the question by presupposing that modern principle of self-determination is the only legitimate axis for evaluating kernels, thus foreclosing covenant and existential readings before analyzing them?',
    'Meta-analysis of legitimacy frameworks: are the readings truly coexisting alternatives within one kernel, or does the self-determination reading contain an implicit claim to meta-legitimacy (modern law trumps divine covenant and existential claims) that would be a foreclosure relation if made explicit?',
    'If the reading implicitly assumes modern international law is the supreme legitimacy framework, it forecloses covenant and existential readings by epistemic fiat rather than engaging them. This would reveal a commitment to liberal internationalism as the authority-grounding that should be made explicit. If the readings are genuinely coexisting, they remain live alternatives without hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether the self-determination reading''s legitimacy framework is coexisting or foreclosing toward sibling readings.').

omega_variable(
    extraction_vs_coordination_separation,
    'Can the self-determination principle''s coordination function (fair allocation of territorial claims based on resident-population will) be separated from the extraction component (territorial dispossession and occupation in service of enforcing the principle against opposed parties)?',
    'Counterfactual analysis: if the self-determination principle were applied without military occupation, settlement restriction, or refugee return denial—only through negotiated majority-rule governance—would the coordination function persist while extraction diminishes? Or is extraction structurally inseparable from enforcing the principle against the Israeli state''s opposed reading?',
    'If separable, the reading describes a tangled_rope where both components could be disentangled through institutional design (majority-rule without occupation/settlement). If inseparable, the reading is a snare that uses coordination language to justify zero-sum territorial extraction from an opposed population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_separation, empirical, 'Whether the self-determination principle''s coordination and extraction components are structurally inseparable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t5, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(terr_tr_t5, observed).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(terr_tr_t10, observed).
narrative_ontology:measurement(terr_tr_t15, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(terr_tr_t15, observed).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(terr_tr_t20, observed).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(terr_tr_t25, observed).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(terr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t5, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement_basis(terr_be_t5, observed).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(terr_be_t10, observed).
narrative_ontology:measurement(terr_be_t15, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(terr_be_t15, observed).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(terr_be_t20, observed).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(terr_be_t25, observed).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(terr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t5, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement_basis(terr_su_t5, observed).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(terr_su_t10, observed).
narrative_ontology:measurement(terr_su_t15, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(terr_su_t15, observed).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(terr_su_t20, observed).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(terr_su_t25, observed).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(terr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__self_determination_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel territorial_sovereignty_legitimacy. Sibling readings instantiate the covenant_continuity_reading and existential_matrix_reading, each grounding legitimacy in a different source (ancient divine covenant, existential survival necessity). The three readings coexist as live positions held by different parties and international constituencies; no single framework adjudicates between them without prior commitments about legitimacy grounding. All three share the same referent (the territorial arrangement in Israel/Palestine) but produce structurally distinct constraints with different beneficiary/victim sets, extraction mechanisms, and persistence conditions. The readings influence each other through diplomatic pressure, military conflict, and international law contestation, but do not logically foreclose one another—they are structurally coexisting alternatives within an irreducibly contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
