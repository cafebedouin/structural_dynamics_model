% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Text-based Protected Personal Self-Defense
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the individual right reading of the Second
 *   Amendment. The operative clause ('the right of the people to keep and
 *   bear arms, shall not be infringed') is read as textually prior to and
 *   independent of the prefatory militia clause ('A well regulated militia,
 *   being necessary to the security of a free state'). Under this reading,
 *   the right to possess firearms for personal self-defense is a fundamental
 *   individual right not conditioned on militia service or state
 *   authorization. The reading beneficiaries are gun owners; the victims are
 *   those legally disarmed (felons, domestic abusers) and
 *   permitting/licensing regimes treated as impermissible conditions. The
 *   measurement series track rising suppression (enforcement against
 *   licensing advocates) and extractiveness (growing constitutional
 *   impediment to regulatory schemes) post-2008, plateauing as the reading's
 *   institutional position solidified. The claim/metric gap is not a mistake:
 *   the reading is CLAIMED as tangled_rope (genuine self-defense coordination
 *   + asymmetric restriction of regulatory authority) while the metrics
 *   honestly describe how the reading's enforcement actively suppresses
 *   competing regulatory regimes and extracts authority from state
 *   bureaucracies—the engine will compute whether that asymmetry holds
 *   structurally.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary beneficiaries (constitutional right secured, permitting barriers removed)
 *   - disarmed_populations: primary victims (categorical exclusion from right-bearing, no alternative protection path)
 *   - permit_and_licensing_regimes: payers (regulatory authority withdrawn, apparatus legally undermined)
 *   - courts_and_interpreters: agenda setters (instantiate the reading through adjudication, certify operative-clause priority)
 *   - public_safety_advocates: observers (constrained by the reading but not excluded from framing; policy tools delegitimized)
 *   - collective_security_reading advocates: excluded (their constitutional reading is foreclosed by operative-clause-first hierarchy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Individual Right Reading: Text-based Protected Personal Self-Defense").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '6d82cf72-0999-4e4e-b85f-13b05ec4d08a').
narrative_ontology:cs_kernel_codification('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', fixed_text).
narrative_ontology:cs_authority_grounding('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', lineage).
narrative_ontology:cs_interpretation_layer_present('6d82cf72-0999-4e4e-b85f-13b05ec4d08a').
narrative_ontology:cs_reading_relation('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', foundational, operative_clause_textual_priority).
narrative_ontology:cs_axiom_status(operative_clause_textual_priority, holdable).
narrative_ontology:cs_axiom_grounding('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', operative_clause_textual_priority, conventional).
narrative_ontology:cs_axiom('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', foundational, personal_self_defense_core_right).
narrative_ontology:cs_axiom_status(personal_self_defense_core_right, holdable).
narrative_ontology:cs_axiom_grounding('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', personal_self_defense_core_right, deontological).
narrative_ontology:cs_reference_frame('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', operative_clause_independence_from_militia_condition).
narrative_ontology:cs_drift_state('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', contemporary_post_heller_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6d82cf72-0999-4e4e-b85f-13b05ec4d08a', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, self_defense_doctrine).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, permit_and_licensing_regimes).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, natural_right_to_self_defense).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_autonomy_from_state_militia_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the Second Amendment as guaranteeing them a constitutional right to acquire and possess firearms for personal self-defense without condition of militia service. They see the operative clause as textually separable from the prefatory militia clause and understand their right as pre-political—grounded in natural law rather than granted by government. They mobilize to resist permitting regimes and licensing regimes, arguing these conditions unconstitutionally restrict the unqualified right.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, mobile, national).

% Convicted felons, persons subject to restraining orders, and individuals with domestic abuse convictions are categorically excluded from firearm possession under this reading's framework. Their exclusion is treated as a permissible categorical restriction (unlike licensing, which this reading treats as impermissible) because the restriction targets status rather than right-bearing capacity itself. They bear the cost of being definitionally outside the protected class and have no meaningful legal recourse under this reading's operative logic.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, biographical, trapped, national).

% State and municipal governments that have enacted permitting and licensing schemes face legal pressure from this reading. They argue their regimes serve public safety by screening applicants; this reading treats such screening as conditioning the exercise of a fundamental right and therefore unconstitutional. They bear the institutional cost of litigation and potential mandatory dismantling of their regulatory apparatus.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, permit_and_licensing_regimes, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, permit_and_licensing_regimes, observer).

% Advocates for the competing collective_security_reading (militia clause interpretation) are locked out of the constitutional frame this reading instantiates. They would argue that the operative clause is militia-conditioned, that states retain broad regulation authority for collective safety, and that permitting serves constitutional purposes. This reading's textual priority over the prefatory clause forecloses that argument within the same legal framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, collective_security_reading_advocates, excluded,
    organized, biographical, constrained, national).

% The originalist_civic_virtue_reading frames the founding-era militia as universal armed citizenry and reads the right as protecting citizen-soldier capacity in defense of republican order. This reading disagrees on emphasis: the individual_right_reading anchors protection in personal self-defense (private sphere), while the originalist reading anchors it in civic participation (public sphere). Their sitting side-by-side as coexisting readings reflects institutional stalemate across different judicial and political constituencies.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, originalist_civic_virtue_advocates, excluded,
    organized, biographical, constrained, national).

% Federal courts, particularly the Supreme Court, institutionalize and enforce this reading through constitutional adjudication. They author the operative-clause-first textual hierarchy, decide which restrictions are impermissible, and determine whether a right is 'fundamentally connected to the natural right to self defense' (per District of Columbia v. Heller, 554 U.S. 570 (2008)). They carry the power to certify the reading as constitutional law.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, courts_and_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Actors focusing on firearm injury prevention, gun violence reduction, and public health outcomes observe this reading as reducing their policy options. They argue that the individual right reading, as judicially certified, strips them of tools (permitting, licensing, waiting periods, capacity restrictions) they consider central to public safety strategy. They are not excluded from the frame but are constrained by it—their interests are not centered in the constitutional value hierarchy this reading instantiates.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_safety_advocates, observer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects individual citizens against both criminal predation and governmental disarmament by securing a private right to self-defense. Coordinates a reading of the constitutional text such that the operative clause stands independently from militia framing, allowing citizens to invoke the right for personal protection without arguing service in organized militia.
% TRANSFER_FUNCTION: Allocates constitutional protection and restraint authority asymmetrically: protection flows to individual gun owners (constitutional right to acquire arms), while permission-authority is withdrawn from state regulatory regimes (permitting and licensing become impermissible conditions on the exercise of a fundamental right). The transfer moves decision-power from state bureaucracies to individual gun owners.
% ABSENT_VOICES: Proponents of the collective_security_reading (militia-conditioned interpretation) are not seated at the doctrinal table this reading constructs—their reading is structurally excluded by the operative-clause-first hierarchy. Public health and injury prevention voices are present as observers but are not empowered within the constitutional value framework: their policy tools (licensing, permitting, waiting periods) are treated as impermissible restrictions on a pre-political right.
% DISAPPEARANCE_RATIONALE: If this reading instantiation—operative-clause-first, individual right independent of militia service—were constitutionally rejected and replaced by the collective_security_reading, the entire landscape of firearm regulation would rearrange. States would recover permitting and licensing authority. The private right would be reread as conditioned on civic militia participation or state-articulated collective security purposes. Individual gun owners would lose the free-standing constitutional shield they presently hold. Courts would certify a different constitutional doctrine. Absent this reading, the constitutional protection of personal self-defense would shrink or vanish.
% FOUNDING_PROBLEM: Citizens require protection against both common criminals and governmental tyranny. The founding-era concern was that governments might disarm populations to prevent resistance; a reading that preserves individual firearm possession independent of state militia control addresses that concern. The operative clause of the Second Amendment is read as textually establishing this protection.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Joyce Lee Malcolm, Randy Barnett) and the Supreme Court majority in District of Columbia v. Heller (2008) attest that the founding problem—preventing governmental disarmament of citizens capable of self-defense—is still live and continues to justify the individual right. Competing scholars and the pre-2008 jurisprudential consensus (United States v. Miller, 1939) attest that the founding problem was militia-specific and is substantially solved by professional standing armies and state-controlled militia. Legislative testimony from firearms advocates (National Rifle Association) and public health researchers produces opposing accounts; the reading itself is not settled by historians external to the dispute.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.45 (baseline: the reading exists as one interpretive option among competing readings; its extraction is potential, not yet instantiated). It rises to 0.68 by interval end, tracking the degree to which this reading's institutional dominance (post-Heller, 2008) has withdrawn permitting authority from states and concentrated right-bearing power in individual decision-making. Suppression stays high and stable (0.72 final): the enforcement machinery required to suppress the competing collective_security_reading and to prevent regulatory alternatives from re-establishing permitting is substantial and non-declining. Theater ratio (0.42 final) reflects moderate performative activity: the reading carries genuine protection function (individuals do gain constitutional shield against some regulatory incursions) but also performs a rhetorical function (framing permitting as unconstitutional rather than as reasonable regulation). Accessibility_collapse (0.58) is moderate because the individual-right reading is now institutionally established (alternatives less visible to non-specialists) but not hegemonic (competing readings remain live in dissenting opinions and political discourse). Resistance (0.81) is high: public health advocates, states, and urban constituencies actively resist the reading's expansion; the dissent in Heller and later decisions documents sustained, organized resistance. The measurement series share a single time grid: every metric is authored at every examined point, preventing misalignment. Temporal trajectory: extractiveness rises sharply 0–15 (post-Heller consolidation) then plateaus (reading's position stabilized by 2020s). Suppression follows a similar arc but flatter. Theater ratio rises initially (the performative dimension grows as the reading must defend itself against resistance) then stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the individual_gun_owners seat, this reading is a rope: genuine coordination (protection against disarmament, self-defense security) with minimal overhead. From the permit_and_licensing_regimes seat, it is a snare: their authority is extracted through constitutional reinterpretation; they bear the enforcement cost of defending against challenges while losing policy tools. Courts and interpreters experience it as agenda-setting rope (they coordinate a reading, maintain its coherence, defend it against alternatives). Public safety advocates experience it as structurally constraining snare (their tools are removed, their values deprioritized). The engine will compute per-seat types from the structural data; this gap explains why the computed seats should diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are beneficiaries (d near 0.0): they collect the right, face no regulatory barriers under this reading's ideal application, and have high exit mobility—they are subsidized by the constraint. Permit_and_licensing_regimes are targets (d near 1.0): their authority is extracted, their enforcement capacity is delegitimized, and they are institutionally trapped (they cannot simply exit permitting without legislative reversal of the reading itself). Disarmed_populations are trapped targets (d = 1.0): they collect nothing from the reading (explicitly excluded by status), have no exit (their status is identity-locked), and face indefinite disability from the right. Public safety advocates are near-symmetric (d ≈ 0.5): they see modest benefits in some of the reading's protections against arbitrary governmental seizure but bear high costs in lost regulatory tools and institutional deprioritization. The reading's authority derives from textual priority (operative before prefatory); the directionality follows from beneficiary/victim structure: gun owners benefit, regulatory regimes and disarmed populations bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy—the founding problem (preventing governmental disarmament of capable citizens) is still live and actively vindicated by continuing litigation and political mobilization. However, there is a mandatrophy candidate within the structure: the treatment of disarmed populations. The original concern was governmental tyranny over the armed citizenry; the exclusion of felons and domestic abusers is treated as a categorical restriction (permissible) rather than a licensing condition (impermissible) because the restriction targets status, not right-bearing capacity. This classification is somewhat arbitrary—it preserves the core right for the 'militia-ready' class while excluding those deemed dangerous or untrustworthy. The omega variables address whether this distinction (categorical status vs. condition) is structurally sound or whether it represents mandate creep—expanding the restriction class beyond the founding problem's original scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_vs_prefatory_priority,
    'Is the operative clause of the Second Amendment textually separable from the prefatory militia clause, or are they semantically unified such that militia service conditions the right?',
    'Linguistic and historical analysis of 18th-century English legal syntax; comparative study of other constitutional provisions using similar structure; evidence of founding-era drafting intent regarding clause order and relationship.',
    'If the clauses are inseparable, the militia condition stands and the collective_security_reading is architecturally superior. If they are separable, the individual_right_reading''s textual foundation is sound. This is the foundational architectural disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operative_vs_prefatory_priority, conceptual, 'Whether operative and prefatory clauses are textually unified or separable.').

omega_variable(
    categorical_vs_conditional_restrictions,
    'Is the distinction between categorical status restrictions (felons, domestic abusers are excluded by status) and conditional licensing restrictions (permitting conditions the exercise of the right) structural or arbitrary?',
    'Sustained judicial application of the distinction; consistency of the courts'' line-drawing across cases; whether the line tracks a coherent principle or drifts with political pressure.',
    'If the distinction is structural, disarmed populations are legitimately treated as outside the protected class. If arbitrary, the classification represents mandate expansion—the restriction class grows beyond the founding problem''s original scope (tyranny of government over capable citizens). This distinction is crucial to classifying disarmed_populations as victims_by_status rather than victims_of_licensing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_conditional_restrictions, empirical, 'Whether categorical and conditional restrictions represent a principled distinction or regulatory drift.').

omega_variable(
    reading_institutional_displacement,
    'Is the individual_right_reading''s institutional dominance (post-Heller) a settled constitutional position, or does it face foreclosure risk from legislative amendment, constitutional reinterpretation, or demographic shift in judicial composition?',
    'Monitoring of Supreme Court composition shifts, state legislative responses (attempting to pass new Second Amendment-protective legislation or gun safety measures), and lower court decisions that either expand or contract the reading''s scope.',
    'If foreclosure risk is substantial, the reading''s current extraction and suppression metrics are contingent on short-term institutional alignment. If the reading is settled, the metrics reflect a durable constraint. This determines whether the reading''s current position should be modeled as stable or as facing phase transition risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_institutional_displacement, empirical, 'Whether the individual right reading is institutionally settled or faces foreclosure risk.').

omega_variable(
    self_defense_vs_civic_virtue_grounding,
    'Is the core protected activity personal self-defense (private sphere), civic militia participation (public sphere), or both equally?',
    'Analysis of the reading''s case law: which activities are protected (home defense, carry for personal protection, militia-relevant arms like rifles, ammunition); which are not (felons, domestic abusers, minors); whether the courts treat self-defense as the primary value or as one value among several.',
    'If personal self-defense is primary, the individual_right_reading is structurally distinct from and incompatible with the originalist_civic_virtue_reading (forecloses it). If civic virtue is equally primary, the readings coexist more easily (both emphasize individual capacity but disagree on its purpose). This determines the reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_vs_civic_virtue_grounding, conceptual, 'Whether self-defense or civic participation is the reading''s foundational protected activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, projected).
narrative_ontology:measurement(seco_tr_t5, second_amendment_text__individual_right_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(seco_tr_t5, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__individual_right_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_text__individual_right_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__individual_right_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t25, second_amendment_text__individual_right_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(seco_tr_t25, observed).
narrative_ontology:measurement(seco_tr_t35, second_amendment_text__individual_right_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(seco_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__individual_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(seco_be_t0, projected).
narrative_ontology:measurement(seco_be_t5, second_amendment_text__individual_right_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(seco_be_t5, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__individual_right_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_text__individual_right_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__individual_right_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t25, second_amendment_text__individual_right_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(seco_be_t25, observed).
narrative_ontology:measurement(seco_be_t35, second_amendment_text__individual_right_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(seco_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__individual_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(seco_su_t0, projected).
narrative_ontology:measurement(seco_su_t5, second_amendment_text__individual_right_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(seco_su_t5, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__individual_right_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_text__individual_right_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__individual_right_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t25, second_amendment_text__individual_right_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(seco_su_t25, observed).
narrative_ontology:measurement(seco_su_t35, second_amendment_text__individual_right_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(seco_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, handgun_licensing_regimes).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, felon_disarmament_categorical_restrictions).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel second_amendment_text. The collective_security_reading and originalist_civic_virtue_reading are sibling constraints instantiating different constitutional readings of the same text. This family exhibits the ε-invariance decomposition pattern: the individual_right_reading has substantial extractiveness (0.68, measured against permitting regimes); the collective_security_reading would have lower extractiveness (permitting is treated as constitutional); the originalist_civic_virtue_reading has moderate extractiveness (civic virtue framing permits some restrictions but not licensing). Each reading is a separate constraint with its own ε, because the observable (whether the operative clause is militia-conditioned) changes the classification outcome. They are linked via network.affects_constraints to reflect that institutional dominance of one reading affects the operating space of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
