% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction: Sovereigntist Reading
 *   domain: international_law/treaty_interpretation
 *
 * SUMMARY:
 *   The Rome Statute creates a treaty framework in which 123 consenting
 *   states delegate prosecution of core international crimes to the
 *   International Criminal Court. The sovereigntist reading interprets this
 *   framework as fundamentally conditioned on state consent: the ICC's
 *   authority is derivative from state parties' delegation, not universal;
 *   non-consenting states and their nationals remain outside the jurisdiction
 *   except when the UN Security Council intervenes. This reading emphasizes
 *   complementarity (national courts retain primary responsibility), state
 *   sovereignty (withdrawal is permitted, consent is revocable), and the
 *   asymmetry created by non-participation of major powers. The constraint is
 *   CLAIMED as rope (genuine coordination among consenting parties) and the
 *   authored metrics describe a coordination mechanism with low
 *   extractiveness and suppression — the sovereigntist reading frames the
 *   Statute as a collaborative institutional choice, not a coercive system.
 *   The kernel context: this constraint is one reading of the Rome Statute's
 *   jurisdictional architecture; it coexists with universalist and hybrid
 *   readings that contest whether the Statute intends universal criminal
 *   accountability transcending consent, or balances consent with
 *   institutional aspiration.
 *
 * KEY AGENTS:
 *   - consenting_state_parties: 123 nations voluntarily bound to ICC jurisdiction; frame the Statute as a coordination of like-minded sovereigns
 *   - icc_institution: operates as a delegated court of consenting parties, not as an independent universal judge
 *   - non_consenting_major_powers: retain strategic autonomy by refusing to ratify; block the universalist reading through non-participation
 *   - national_judiciaries: maintain primary responsibility under complementarity; the ICC is a backstop, not a supercessor
 *   - unsc_permanent_members: hold gatekeeping power to refer non-party situations, embedding a hierarchical inequality into the sovereigntist framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.38).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.22).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction: Sovereigntist Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab').
narrative_ontology:cs_kernel_codification('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', fixed_text).
narrative_ontology:cs_authority_grounding('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', lineage).
narrative_ontology:cs_interpretation_layer_present('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab').
narrative_ontology:cs_reading_relation('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', foundational, state_consent_is_jurisdictional_foundation).
narrative_ontology:cs_axiom_status(state_consent_is_jurisdictional_foundation, holdable).
narrative_ontology:cs_axiom_grounding('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', state_consent_is_jurisdictional_foundation, conventional).
narrative_ontology:cs_axiom('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', foundational, complementarity_as_genuine_deference).
narrative_ontology:cs_axiom_status(complementarity_as_genuine_deference, holdable).
narrative_ontology:cs_axiom_grounding('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', complementarity_as_genuine_deference, conventional).
narrative_ontology:cs_reference_frame('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', treaty_based_delegation_by_consenting_sovereigns).
narrative_ontology:cs_drift_state('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', contemporary_geopolitical_gridlock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6df9ade3-b738-4ebe-bcb0-c9aecc4b1fab', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, state_national_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, crime_victims_in_party_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, treaty_pacta_sunt_servanda).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, state_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 123 states have voluntarily ratified or acceded to the Rome Statute. They frame their participation as a principled commitment to international accountability while retaining the strategic autonomy to withdraw (six-month notice under Article 127). They set the Statute's rules through the Assembly of States Parties and control the crimes that trigger ICC jurisdiction over their own nationals through amendments and interpretive practice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_parties, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_parties, agenda_setter).

% The International Criminal Court administers the Rome Statute framework as a delegated institution. Under the sovereigntist reading, the ICC operates with authority conditional on state parties' delegation, not as an independent universal judge. It lacks power to compel non-state-party nationals into proceedings except via UN Security Council referral (Article 13(b)). The Court's complementarity jurisdiction requires state exhaustion or demonstrated unwillingness/inability, preserving national judicial primacy.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_institution, agenda_setter,
    institutional, generational, constrained, global).

% The United States, Russia, China, India, and other significant non-parties have declined to ratify the Rome Statute. Under the sovereigntist reading, their nationals are immune from ICC jurisdiction absent UNSC referral — a rare event requiring consensus from these same powers. Their non-participation reflects active rejection of the consent-based framework itself; they view universal ICC jurisdiction as incompatible with strategic autonomy and sovereign equality.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_consenting_major_powers, excluded,
    institutional, generational, mobile, global).

% National courts retain primary responsibility for prosecuting Rome Statute crimes under the complementarity principle. The ICC exercises jurisdiction only when domestic proceedings are unable or unwilling to provide genuine investigation or prosecution. This structure preserves national judicial sovereignty and roots accountability in local legal systems, where both perpetrators and victims reside.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, global).

% Citizens of ICC state parties gain potential recourse to international prosecution when domestic courts fail to act. This benefit is conditional: it depends on both national incapacity or unwillingness AND ICC prosecutor discretion (limited by finite resources). For victims in non-party states, the sovereigntist reading offers no ICC recourse unless the UNSC intervenes, leaving large populations (in non-consenting major powers' territories) outside the protection mechanism.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, crime_victims_in_party_states, beneficiary,
    powerless, biographical, trapped, global).

% The five UNSC permanent members (US, Russia, China, UK, France) hold gatekeeping authority under Article 13(b): they can refer situations outside ICC jurisdiction to the prosecutor, effectively extending the ICC's reach to non-consenting states. Conversely, any permanent member can block or veto referrals of situations involving their own strategic interests (implicit veto threat). This structural asymmetry means the ICC operates with an embedded inequality: powerful, non-consenting states retain the power to invoke or prevent ICC jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic and expert communities analyze whether the Rome Statute's text, travaux préparatoires, and state practice support the sovereigntist, universalist, or hybrid reading. Their interpretive work feeds jurisdictional adjudication, treaty amendment debates, and institutional reforms, but they hold no direct governance power.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, diffuse).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a treaty-based institutional framework through which consenting sovereign states coordinate prosecution of core international crimes (genocide, crimes against humanity, war crimes, aggression) that are difficult or politically impossible to prosecute domestically. The mechanism creates a court of last resort when complementarity fails, avoiding duplicative enforcement while respecting national judicial primacy.
% TRANSFER_FUNCTION: States parties transfer delegation of prosecution authority to the ICC in exchange for reciprocal jurisdiction: each ratifying state submits itself to potential ICC prosecution while gaining the right to refer other states' nationals to ICC proceedings (within the complementarity gate). This is a reciprocal transfer conditioned on voluntary entry; non-consenting states remain outside the flow.
% ABSENT_VOICES: Non-consenting major powers (US, Russia, China, India) reject the sovereigntist framing of the Statute itself — they would argue either for total exemption from any international criminal jurisdiction ('strategic autonomy') or for a fundamentally different institutional design (universal jurisdiction administered by a different body, not requiring consent). Victims in conflict zones involving non-party states have no institutional voice in the consent framework, though many would advocate for expanded ICC reach to cover their situations.
% DISAPPEARANCE_RATIONALE: If the Rome Statute framework and ICC disappeared overnight, the sovereigntist reading asserts the world would reorganize around national courts as the primary accountability mechanism. International prosecutions would fragment by state capacity and political will: well-resourced democracies might vigorously prosecute; authoritarian regimes and weak states would offer safe haven. Ad-hoc tribunals might be created for specific crises (like Kosovo, Syria), but the permanent institutional coordination would disappear. The UNSC could still authorize tribunals, but without the treaty infrastructure, prosecutions would be slower, more politically contingent, and geographically uneven.
% FOUNDING_PROBLEM: Post-World War II, major powers created the Nuremberg and Tokyo tribunals for specific conflicts, then dissolved them when the political moment passed. The post-Cold War period saw similar ad-hoc tribunals for Yugoslavia and Rwanda, then their sunset. The Rome Statute was created in 1998 to establish a permanent institution for future crimes of international concern, with the limitation that it would operate only among consenting states (reflecting the reality that major powers rejected universal binding).
% FOUNDING_PROBLEM_CORROBORATION: States Parties (particularly African and European nations) attest the founding problem is live — ongoing atrocities in Democratic Republic of Congo, Uganda, Sudan, and Palestine demonstrate the need for ICC intervention. Universalist scholars and victims' advocates attest the founding problem persists BECAUSE of the sovereigntist limitation: the ICC's inability to reach non-consenting major powers leaves systematic impunity for their actions. Non-consenting major powers reject the founding problem framing, arguing it reflects a narrow coalition's view of international criminal accountability, not a universal consensus. They further argue that ad-hoc mechanisms or universal jurisdiction in domestic courts (rather than a treaty institution) would be more legitimate.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.38 endpoint) because the sovereigntist reading frames the Statute as a voluntary coordination: states choose to participate, retain exit rights, and benefit from reciprocal jurisdiction. Suppression is similarly low (0.22) — the constraint operates through treaty language and state practice, not through coercive enforcement against unwilling parties. Theater ratio is minimal (0.18) because the sovereigntist reading emphasizes the real function (complementarity as genuine delegation to a court of last resort) over performative aspects. However, the measurement series shows gradual rise over the 28-year interval: extractiveness, suppression, and theater all drift upward. This drift reflects the sovereigntist reading's internal contradiction — as non-consenting powers accumulate and the UNSC becomes less reliable, the consent-based framing appears increasingly theatrical (the Statute is mandatory for 123 states but optional for those powerful enough to reject it). Accessibility_collapse (0.65) reflects that once the sovereigntist framework is understood, alternatives (universal jurisdiction, ad-hoc tribunals, bilateral extradition treaties) remain available but require exit from the Statute or institutional reform. Resistance (0.71) captures ongoing contestation: universalist scholars, victims' advocates, and some African states dispute the sovereigntist limitation and advocate for expanded ICC reach.
 *
 * PERSPECTIVAL GAP:
 *   Consenting state parties and the ICC institution experience this constraint differently from non-consenting powers. From a state party's position, the Statute is genuine coordination — voluntary entry into a system that binds all participants equally under treaty law. From a non-consenting major power's position, the Statute is a constraint imposed on others that this power refuses: the sovereigntist reading vindicates their choice to stay out. The engine computes these divergent directionalities from the structural data (beneficiary vs. excluded stakeholder, arbitrage vs. trapped exit). From the UNSC's permanent members' perspective, the sovereigntist reading grants them asymmetric power (referral authority without automatic submission), embedding a hierarchy that differentiates their exit options from those of smaller states. The authored metrics reflect the state-party viewpoint (low extraction, genuine coordination); from the excluded-power perspective, the metrics would appear differently (high extraction of legitimacy through selective binding).
 *
 * DIRECTIONALITY LOGIC:
 *   Consenting state parties are beneficiaries: they gain reciprocal jurisdiction over crimes and retain strategic autonomy through withdrawal options (d near beneficiary end, ~0.25). The ICC institution is an agenda-setter but with constrained authority — it operates within the delegated authority consenting states grant (moderate d, ~0.45). National judiciaries are beneficiaries (complementarity defers to them) but with constrained sovereignty (international oversight of willingness/ability). Crime victims in party states are beneficiaries but at the mercy of state unwillingness and ICC discretion (moderate d, ~0.50). Non-consenting major powers are excluded and face a directional inversion: the Statute creates costs for them (liability risk for nationals if UNSC acts) without benefits (no guaranteed access to prosecution of their enemies' crimes), but their power to block UNSC referrals mitigates the cost (d at the target end but limited by gatekeeping power, ~0.65). The UNSC permanent members sit at asymmetry: gatekeeping authority over non-party situations (d near beneficiary for referral power, ~0.20) but submission risk for their own citizens if referral succeeds (~0.60 when a referral includes themselves).
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading does NOT exhibit mandatrophy. The founding problem (post-conflict accountability, permanent institution for crimes of international concern) remains live — new prosecutions occur regularly, complementarity is genuinely applied, and victim participation mechanisms function. The constraint is not merely performed or theatrically maintained. However, the rising theater_ratio over the interval (from 0.08 to 0.18) suggests a secondary dynamic: as major-power non-participation becomes entrenched and UNSC referrals become rarer (geopolitical gridlock increases), the sovereigntist reading's promise of universal accountability for grave crimes becomes increasingly theatrical — the Statute appears to bind only willing states while the worst offenders (major powers) escape jurisdiction. This is not mandatrophy (the founding problem is not dead), but it is drift toward a situation where the sovereigntist framework's legitimacy depends increasingly on the fiction of eventual universality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_customary_international_law,
    'Does the sovereigntist reading''s emphasis on treaty consent conflict with emerging customary international law norms that attribute universal criminal accountability for core crimes (genocide, crimes against humanity) independent of state consent?',
    'Future ICJ or arbitral decisions on whether customary norms bind non-consenting states; emergence of universal jurisdiction exercises in domestic courts; UNSC practice on referrals.',
    'If customary norms override consent-based treaties, the sovereigntist reading''s foundation (consent as the jurisdictional gate) would be undermined, and the constraint would drift toward the universalist reading. If customary norms remain subordinate to state consent, the sovereigntist reading is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_vs_customary_international_law, empirical, 'Whether emerging customary norms override the consent-based treaty framework.').

omega_variable(
    major_power_ratification_trajectory,
    'Will non-consenting major powers (US, Russia, China, India) ratify the Rome Statute, or will their non-participation become permanent?',
    'Observation of state declarations and ratification timelines; analysis of domestic political barriers (US Senate opposition, Russian strategic autonomy doctrine, Chinese sovereignty doctrine).',
    'If major powers ratify, the sovereigntist reading''s asymmetry diminishes and the Statute approaches functional universality. If non-participation persists, the sovereigntist reading remains structurally accurate but increasingly contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(major_power_ratification_trajectory, empirical, 'The long-term trajectory of major-power participation in the Rome Statute regime.').

omega_variable(
    complementarity_as_deference_vs_override,
    'Is complementarity in the sovereigntist reading a genuine principle of deference to national courts, or is it increasingly functioning as a gate the ICC can override when it judges national proceedings inadequate?',
    'ICC prosecutor decisions and Pre-Trial Chamber rulings on admissibility; comparison of statutory language (''unable or unwilling'') with judicial practice; state responses to ICC determinations of inadequacy.',
    'If complementarity remains genuine deference, the sovereigntist reading is accurate: national courts retain primary authority. If the ICC increasingly overrides national determinations, the constraint drifts toward the universalist reading despite formal consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_deference_vs_override, empirical, 'Whether complementarity functions as deference or as an ICC override mechanism.').

omega_variable(
    unsc_referral_gatekeeping_asymmetry,
    'Is the UNSC permanent-member gatekeeping over non-party situations a feature of the sovereigntist framework or a corruption of the consent principle?',
    'Debate on ICC reform: whether to expand the prosecutor''s proprio moto powers over non-consenting states or to maintain UNSC gatekeeping as a check on universalism. State practice on referral acceptance and blocking.',
    'If UNSC gatekeeping is reframed as a necessary constraint on institutional overreach, it strengthens the sovereigntist reading. If it is reframed as an unjustifiable inequality that enables major-power impunity, pressure will grow to eliminate it, pushing the framework toward universalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_gatekeeping_asymmetry, conceptual, 'The legitimacy and role of UNSC gatekeeping in the consent-based framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement_basis(rome_tr_t1998, projected).
narrative_ontology:measurement(rome_tr_t2005, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement_basis(rome_tr_t2005, observed).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2010, 0.13).
narrative_ontology:measurement_basis(rome_tr_t2010, observed).
narrative_ontology:measurement(rome_tr_t2015, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement_basis(rome_tr_t2015, observed).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement_basis(rome_tr_t2020, observed).
narrative_ontology:measurement(rome_tr_t2026, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(rome_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.12).
narrative_ontology:measurement_basis(rome_be_t1998, projected).
narrative_ontology:measurement(rome_be_t2005, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement_basis(rome_be_t2005, observed).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement_basis(rome_be_t2010, observed).
narrative_ontology:measurement(rome_be_t2015, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2015, 0.31).
narrative_ontology:measurement_basis(rome_be_t2015, observed).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement_basis(rome_be_t2020, observed).
narrative_ontology:measurement(rome_be_t2026, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(rome_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.15).
narrative_ontology:measurement_basis(rome_su_t1998, projected).
narrative_ontology:measurement(rome_su_t2005, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement_basis(rome_su_t2005, observed).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement_basis(rome_su_t2010, observed).
narrative_ontology:measurement(rome_su_t2015, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2015, 0.21).
narrative_ontology:measurement_basis(rome_su_t2015, observed).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement_basis(rome_su_t2020, observed).
narrative_ontology:measurement(rome_su_t2026, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(rome_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint (sovereigntist reading) is part of a kernel family decomposed from the Rome Statute's contested jurisdictional mandate. Three readings of the same statutory text produce structurally distinct constraints: the sovereigntist reading emphasizes consent-based delegation and national primacy; the universalist reading emphasizes universal accountability transcending consent; the hybrid reading balances both. Each reading has distinct ε, beneficiary/victim structures, and directionality profiles. The sibling readings are linked via network.affects_constraints to enable cross-reading analysis of institutional drift and legitimacy contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__sovereigntist_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
