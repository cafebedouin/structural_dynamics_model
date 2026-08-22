% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity: International Oversight Reading
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes complementarity: the ICC's
 *   jurisdiction is triggered when national courts are 'unwilling or unable'
 *   to prosecute genuinely. This story instantiates the
 *   international-oversight reading: 'unwilling or unable' is interpreted
 *   broadly to capture elite capture, victor's justice, and performative
 *   prosecutions in weak or complicit states. Under this reading, the ICC
 *   acts as a guardian against impunity, with a low threshold for concluding
 *   that domestic proceedings are inadequate. The beneficiary is victims in
 *   captured states; the cost-bearer is state sovereignty and elites shielded
 *   by national capture. The reading is structurally tangled: the ICC
 *   coordination function (establishing accountability mechanisms) and the
 *   extraction mechanism (overriding state prerogatives and threatening
 *   elites) ride the same rule. The claim/metric independence principle
 *   applies: this is claimed as tangled_rope (genuine coordination +
 *   asymmetric enforcement) while the authored metrics describe substantial
 *   extraction and performative activity at the margins—the divergence is
 *   diagnostic of the reading's character.
 *
 * KEY AGENTS:
 *   - International Criminal Court — interprets and implements complementarity with broad threshold; agenda-setter
 *   - Victims in complicit/weak states — gain recourse when national courts fail; beneficiaries but structurally powerless
 *   - National sovereigns under scrutiny — states whose legal systems face ICC override when deemed inadequate; payers
 *   - Elites in weak states — government and military officials exposed to ICC prosecution when national immunity fails; payers
 *   - Powerful states and their aligned blocs — excluded from real enforcement risk despite theoretical equivalence; structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.62).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.71).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity: International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '738945fc-701e-4a5d-bd57-13d0bd2f0226').
narrative_ontology:cs_kernel_codification('738945fc-701e-4a5d-bd57-13d0bd2f0226', fixed_text).
narrative_ontology:cs_authority_grounding('738945fc-701e-4a5d-bd57-13d0bd2f0226', lineage).
narrative_ontology:cs_interpretation_layer_present('738945fc-701e-4a5d-bd57-13d0bd2f0226').
narrative_ontology:cs_reading_relation('738945fc-701e-4a5d-bd57-13d0bd2f0226', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('738945fc-701e-4a5d-bd57-13d0bd2f0226', foundational, state_capture_presumptively_triggers_icc_jurisdiction).
narrative_ontology:cs_axiom_status(state_capture_presumptively_triggers_icc_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('738945fc-701e-4a5d-bd57-13d0bd2f0226', state_capture_presumptively_triggers_icc_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('738945fc-701e-4a5d-bd57-13d0bd2f0226', foundational, international_accountability_trumps_state_autonomy).
narrative_ontology:cs_axiom_status(international_accountability_trumps_state_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('738945fc-701e-4a5d-bd57-13d0bd2f0226', international_accountability_trumps_state_autonomy, deontological).
narrative_ontology:cs_reference_frame('738945fc-701e-4a5d-bd57-13d0bd2f0226', broad_complementarity_accountability_trigger).
narrative_ontology:cs_drift_state('738945fc-701e-4a5d-bd57-13d0bd2f0226', contemporary_great_power_asymmetry_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('738945fc-701e-4a5d-bd57-13d0bd2f0226', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_legal_order).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_sovereigns_under_scrutiny).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, elites_in_weak_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, great_powers_and_aligned_blocs).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, security_council_members).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, universal_human_rights_supremacy).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, accountability_without_borders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements Article 17 complementarity with a broad threshold for concluding states are 'unwilling or unable.' Sets evidentiary standards for adequacy of national proceedings. Initiates investigations when threshold is crossed. Controls the operational definition of which states qualify for ICC jurisdiction. Benefits institutionally from broad interpretation (expanded mandate) and normatively from positioning itself as guardian against impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, global).

% Populations harmed in mass atrocities where national courts are captured by the perpetrators or too weak to prosecute independently. Under this reading, they gain access to international prosecution when domestic justice fails. Cannot opt out of the jurisdiction or exit the state. Their primary recourse is the ICC's broad interpretation and willingness to intervene. Have suffered grievous harm; their structural position is entirely dependent on external enforcement.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, global).

% States whose institutions are subject to ICC scrutiny and potential override when deemed 'unwilling or unable' under the broad interpretation. Must cooperate with ICC investigations, surrender suspects, and accept findings of inadequacy in their own judicial processes. Cannot exit the ICC without severe geopolitical isolation; bound by the treaty even when its interpretation is contested.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_sovereigns_under_scrutiny, payer,
    powerful, generational, constrained, global).

% Government and military officials in weak or captured-court states. Under this reading, their immunity from national systems can be stripped if the ICC determines those systems are unwilling or unable to prosecute genuinely. They cannot resign from their professional/political role without abandoning their power base and income; their identity is fused with their position in the state apparatus. Face personal criminal exposure on the basis of external judgment.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, elites_in_weak_states, payer,
    organized, biographical, identity_locked, global).

% Permanent Security Council members and their political allies benefit from the structural exemption built into the Rome Statute (Art. 16 deferral, Security Council veto on ICC actions, non-cooperation arrangements) while rhetorically supporting the broad complementarity interpretation applied to weaker states. Officially subject to the same threshold but practically exempt from enforcement. Their officials do not face ICC investigation despite theoretical equivalence to weak-state elites.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, great_powers_and_aligned_blocs, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, great_powers_and_aligned_blocs, excluded).

% Academic and professional analysts who interpret the scope and validity of the broad complementarity reading. Assess whether it reflects the statute's intent, accords with state practice, and produces legitimate outcomes. Generate interpretive arguments that ICC and states reference in their own debates.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_legal_scholars, observer,
    institutional, generational, analytical, global).

% Civil society actors and transitional justice experts in post-conflict societies who believe national courts—even imperfect ones—should have primary responsibility for prosecuting atrocities. Argue for domestic capacity-building and local legitimacy. Marginalized by the broad interpretation's presumption that national systems are likely inadequate. Their voice is structurally excluded from the decision-making on whether ICC intervention is warranted.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, domestic_justice_advocates, excluded,
    moderate, biographical, constrained, national).

% Can defer ICC investigations of their allies via Article 16 and veto interventions in situations they designate. Control the conditions under which the broad complementarity interpretation is applied. Benefit from the constraint's asymmetry without explicitly endorsing it. Set the limits of ICC authority through institutional veto.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, security_council_members, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, security_council_members, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a supranational accountability mechanism for mass atrocities committed in states where national courts are too weak, captured by perpetrators, or politically unable to prosecute independently. Coordinates global commitment to ensuring that impunity is not a consequence of state failure or complicity.
% TRANSFER_FUNCTION: Transfers prosecutorial authority from national sovereigns to the ICC when the ICC determines (under the broad interpretation) that states are 'unwilling or unable' to prosecute genuinely. Transfers investigation costs and legal burdens to the international system. Transfers authority to adjudicate the adequacy of national proceedings from national legislatures to the ICC and international law. Transfers the cost of non-cooperation (isolation, reputational loss) to non-compliant states.
% ABSENT_VOICES: National courts in weak states (whose independence and adequacy are questioned by this reading) are formally in the conversation but structurally subordinate to ICC review. Domestic transitional justice advocates are excluded. The elites facing prosecution have no vote in determining what standard applies. Great powers whose officials are theoretically subject to the same standard are structurally absent from the enforcement mechanism. Post-conflict reconciliation experts in affected states are marginalized by the reading's presumption of systemic inadequacy.
% DISAPPEARANCE_RATIONALE: If this reading and the broad complementarity interpretation disappeared, the ICC's jurisdiction would narrow dramatically. States where national courts are captured would revert to zero external accountability unless those courts reformed. Victims in weak states would lose their primary recourse. International consensus on atrocity accountability would shift from enforcement to persuasion. The institutional position of the ICC would be substantially weakened.
% FOUNDING_PROBLEM: Mass atrocities committed in weak or politically captured states where national courts are unwilling or unable to prosecute, leaving perpetrators with de facto impunity and creating cycles of injustice and renewed violence.
% FOUNDING_PROBLEM_CORROBORATION: Victim advocacy organizations and international human rights groups attest the founding problem remains live and severe. The ICC and its supporting institutions attest the problem is ongoing and requires the broad interpretation to address. National governments of weak and middle-power states, and conservative international law scholars, attest the problem has been overstated and the remedy (broad complementarity) creates new problems of external override and victor's justice. The systematic evidence base is mixed: some regions show persistent impunity for mass atrocities; others show weak national courts gradually developing capacity for independent prosecution; the global incidence of mass atrocities attributable to state capture has not been tracked systematically.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the broad 'unwilling or unable' standard enables the ICC to override state determinations of adequacy, extracting authority over atrocity prosecution from sovereigns and concentrating it at the international level. Suppression is high (0.71) because the constraint requires non-compliant states to cooperate with investigations and arrests or face isolation; exit is costly. Theater is high-moderate (0.48) because the ICC's adjudications about state 'unwillingness' often rest on subjective interpretations of prosecutorial independence and genuine intent—performances of reform that satisfy or fail to satisfy external standards without changing underlying state capacity. The measurement series show extractiveness rising sharply in the early period (0.48→0.62 across 0-20) as the broad complementarity interpretation gained institutional entrenchment, then plateauing as it became the settled practice. Theater ratio rises similarly, suggesting that as ICC scrutiny intensified, states engaged in more performative compliance measures (training prosecutors, enacting new legislation) while structural capture remained. Suppression requirement follows the same trajectory, indicating that compliance enforcement costs accelerated early and then stabilized as states adapted to the institutional environment.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC's institutional seat, the constraint enables accountability where states fail and protects victims—a coordination function. From the seat of a sovereign state under scrutiny, the same constraint represents an external power override that undermines legal autonomy and exposes elites to prosecution on externally-defined criteria—extraction. From the seat of a victim in a captured state, the constraint offers genuine recourse; from the seat of a powerful state, it is theater (the broad interpretation does not apply to their officials, only to weak-state elites). The engine will compute these divergences from the structural data: the ICC and its institutional allies should compute the constraint as rope-weighted (coordination-dominant); sovereigns and elites should compute it as tangled_rope or snare-weighted (extraction-dominant); powerful states should compute it as beneficiary-positioned despite victim rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC holds institutional power and 'exit_options: analytical'—it operates the rule and is not constrained by it; d ≈ 0.0 (pure beneficiary). Victims hold 'powerless' and 'exit_options: trapped'; they benefit but cannot exit or influence the standard; d ≈ 0.85 (asymmetric beneficiary, nearly-target). National sovereigns hold 'powerful' and 'exit_options: constrained'; they pay non-compliance costs and face authority override; d ≈ 0.75 (target). Elites hold 'organized' and 'exit_options: identity_locked' (professional identity binds them to the state apparatus they cannot leave); d ≈ 0.72 (target, partially trapped by identity). Powerful states hold 'institutional' and 'exit_options: constrained' but are structurally exempt from enforcement; override to d ≈ 0.15 (structural asymmetry: nominal target, actual beneficiary via non-enforcement). This directionality override captures the core injustice claim under this reading: the same rule applies globally in principle but selectively in practice, benefiting great powers while extracting from weak-state elites.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (atrocities in weak states left unpunished) is live in the sense that mass atrocities continue and some states remain unwilling or unable to prosecute. But the reading's solution—broad ICC complementarity—is contested. The constraint shows rising extractiveness early and then plateauing, suggesting that the 'unwilling or unable' mechanism has become entrenched rather than dynamically responsive. The high theater ratio indicates that much institutional activity is performative compliance by states (new courts, rhetoric of reform) rather than genuine capacity change. The analysis does not find mandatrophy at the level of the reading itself—the reading remains actively applied—but does identify a secondary mandatrophy risk: if the founding problem's locus shifted (from mass atrocities in weak states to precision strikes by powerful states), the reading's remedies would not reach it, leaving the constraint's justification partially orphaned. This is documented in the omega on secular decline in mass atrocities among the reading's original target population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_unable_threshold_ambiguity,
    'What evidentiary standard and degree of institutional independence qualify a state as ''willing and able'' under Article 17? Does the broad interpretation (low threshold, capture-inclusive) or the narrow interpretation (high threshold, sham-only) better reflect the Vienna Convention''s rules for treaty interpretation?',
    'Systematic comparative analysis of ICC admissibility decisions: do they reveal consistent evidentiary standards, or do they show case-by-case variation suggesting interpretive drift? Historical analysis of the Rome Statute''s negotiating history and commentary on Article 17''s intended scope. Formal legal opinion from treaty-interpretation authorities outside the ICC.',
    'If the broad threshold is authoritatively endorsed, the constraint''s extractiveness is justified by the coordination function. If the threshold is narrowed, the constraint''s extractiveness becomes more difficult to defend as coordination rather than override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_unable_threshold_ambiguity, conceptual, 'Ambiguity in what ''unwilling or unable'' means operationally; the core contest between readings.').

omega_variable(
    victor_justice_risk_under_broad_interpretation,
    'Does the broad ''unwilling or unable'' interpretation create vulnerability to victor''s justice: ICC prosecution of losers in civil wars or geopolitical conflicts while winners escape scrutiny?',
    'Audit of ICC case selection (where cases originated, initiating party, outcome of conflict at time of ICC involvement): does the distribution show overrepresentation of conflicts where victims and perpetrators align with weaker geopolitical actors or disfavored regimes? Interviews with ICC staff and prosecutor on case-selection criteria and institutional constraints. Comparison to cases left unprosecuted where perpetrators are powerful-state aligned.',
    'If victor''s justice bias is demonstrated, the constraint''s claimed coordination function is undermined; it becomes selective extraction. If no bias is found, the broad interpretation retains legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victor_justice_risk_under_broad_interpretation, empirical, 'Whether the broad complementarity reading is applied symmetrically or serves as a tool for asymmetric prosecution of disfavored regimes.').

omega_variable(
    secular_decline_founding_problem,
    'Has the incidence of mass atrocities committed by weak-state governments in situations of state capture declined significantly since 2002 (ICC''s founding), such that the original founding problem is no longer the ICC''s principal caseload?',
    'Statistical analysis of atrocity patterns and perpetrator types (state vs. non-state, weak vs. powerful states) before and after 2002. Categorization of ICC cases by perpetrator type and state fragility. Assessment of whether the constraint now primarily addresses terrorism and insurgent violence rather than state capture.',
    'If the founding problem (mass atrocities in weak captured states) is no longer the ICC''s primary focus, the constraint shows signs of mandatrophy: the problem it was built to solve has partially disappeared, but the mechanism persists and extracts costs elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_decline_founding_problem, empirical, 'Whether the constraint''s founding problem remains live or has been substantially resolved.').

omega_variable(
    structural_asymmetry_great_power_exemption,
    'Is the de facto exemption of great powers and their allies from ICC prosecution structural (embedded in the Rome Statute and institutional arrangements) or contingent (a temporary political outcome)?',
    'Comparative legal analysis of the Rome Statute''s provisions for permanent Security Council members (Art. 16 deferral, structural non-cooperation arrangement) vs. ICC claims of universality. Historical analysis of whether any great-power-aligned elite has faced ICC prosecution. Assessment of whether the reading is actually ''complementarity as accountability-trigger for weak states'' vs. the stated ''complementarity as accountability-trigger for all.''',
    'If asymmetry is structural, the reading''s claimed universality is false; the constraint is extraction from weak elites justified by accountability rhetoric. If asymmetry is contingent, institutional reform could extend the reading to all states equally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_asymmetry_great_power_exemption, empirical, 'Whether the international-oversight reading applies equally to all states or is systematically biased toward weak-state accountability.').

omega_variable(
    kernel_reading_contest_coexistence,
    'Can the international-oversight and national-primacy readings of Article 17 coexist as legitimately held positions within a single interpretive tradition, or does one reading''s core premise foreclose the other?',
    'Formal analysis of the two readings'' axioms: do they contradict at the foundational level, or do they occupy different empirical/normative premises that could both be true under different circumstances? Survey of legal scholarship on whether both readings are considered mainstream and defensible positions.',
    'If the readings coexist (coexist_with relation), both remain live options for different parties and different historical contexts. If the international-oversight reading forecloses the national-primacy reading, the container cannot hold both; one must prevail. This determines whether the constraint is a stable institutional compromise or an unstable over-interpretation waiting for reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_coexistence, conceptual, 'Whether the kernel''s two readings can coexist or whether one logically eliminates the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__international_oversight_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__international_oversight_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__international_oversight_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__international_oversight_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__international_oversight_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__international_oversight_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__international_oversight_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__international_oversight_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__international_oversight_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__international_oversight_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, state_immunity_customary_international_law).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, international_humanitarian_law_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity kernel. The national_primacy_reading is the sibling constraint instantiating the rival reading of the same kernel. These two stories share the same rule text but derive different ε values, beneficiary/victim structures, and classification outcomes based on their interpretive premises. The two readings coexist in live institutional dispute; neither forecloses the other in principle, though each reading has institutional players who treat the other as delegitimized. The network link records the structural kinship: both constraints operate the same legal rule, but they model different readings of what that rule entails. Decomposition follows DP-001 (ε-invariance): different readings instantiate different constraints because the same rule text, measured via different observables (what counts as 'unwilling or unable'), yields materially different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
