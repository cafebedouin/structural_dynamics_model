% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Prohibition Reading of the Substance Control Kernel — Criminalized Use as Moral Transgression
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of the substance control
 *   kernel: substance use is treated as a moral transgression against social
 *   order, warranting state punishment independent of harm to third parties.
 *   Under this reading, criminal statute is the mechanism, enforcement and
 *   incarceration are the apparatus, and users themselves enter the victim
 *   set directly by virtue of the act of use. This is structurally distinct
 *   from the harm_reduction_reading (which treats use as a health condition
 *   and channels state resources toward treatment infrastructure rather than
 *   punishment) and the legalization_reading (which treats use as a liberty
 *   matter bounded only by externality prevention). Each reading has a
 *   different ε, a different beneficiary/victim structure, and a different
 *   primary state posture — coercive-punitive here, versus service-provision
 *   or regulatory-permissive in the siblings. They are not measured
 *   differently; they are different constraints sharing one contested kernel.
 *
 * KEY AGENTS:
 *   - narcotics_enforcement_agencies: Primary agenda-setter and beneficiary (institutional/arbitrage) — collects budget, personnel, and forfeiture revenue from continued criminalization
 *   - substance_using_individuals: Primary target (powerless/trapped) — bears criminal liability for the act of use itself, independent of third-party harm
 *   - private_prison_operators and cartel_organizations: Secondary institutional beneficiaries whose revenue models depend structurally on prohibition's existence
 *   - public_health_researchers: Analytical/excluded observer — sees the comparative outcome data but lacks standing in the statute-setting process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Prohibition Reading of the Substance Control Kernel — Criminalized Use as Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '86a71b9a-a785-4971-bd92-3ca545af7a75').
narrative_ontology:cs_kernel_codification('86a71b9a-a785-4971-bd92-3ca545af7a75', formalized).
narrative_ontology:cs_authority_grounding('86a71b9a-a785-4971-bd92-3ca545af7a75', extraction).
narrative_ontology:cs_interpretation_layer_present('86a71b9a-a785-4971-bd92-3ca545af7a75').
narrative_ontology:cs_reading_relation('86a71b9a-a785-4971-bd92-3ca545af7a75', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('86a71b9a-a785-4971-bd92-3ca545af7a75', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('86a71b9a-a785-4971-bd92-3ca545af7a75', foundational, use_itself_constitutes_moral_wrong).
narrative_ontology:cs_axiom_status(use_itself_constitutes_moral_wrong, holdable).
narrative_ontology:cs_axiom_grounding('86a71b9a-a785-4971-bd92-3ca545af7a75', use_itself_constitutes_moral_wrong, deontological).
narrative_ontology:cs_axiom('86a71b9a-a785-4971-bd92-3ca545af7a75', secondary, state_punishment_restores_social_order).
narrative_ontology:cs_axiom_status(state_punishment_restores_social_order, holdable).
narrative_ontology:cs_axiom_grounding('86a71b9a-a785-4971-bd92-3ca545af7a75', state_punishment_restores_social_order, instrumental).
narrative_ontology:cs_reference_frame('86a71b9a-a785-4971-bd92-3ca545af7a75', moral_order_criminalization_baseline).
narrative_ontology:cs_drift_state('86a71b9a-a785-4971-bd92-3ca545af7a75', contemporary_overdose_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('86a71b9a-a785-4971-bd92-3ca545af7a75', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, narcotics_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, asset_forfeiture_units).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, cartel_organizations).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_using_individuals).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, convicted_nonviolent_offenders).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, communities_of_color).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, families_of_incarcerated_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, bystanders_in_black_market_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, moral_order_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces criminal statutes against possession, sale, and use. Justifies budget growth, personnel expansion, and asset forfeiture authority through the moral-transgression framing. Its institutional survival and funding are directly tied to the continuation of criminalization; decriminalization would eliminate the agency's core mandate.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, narcotics_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, narcotics_enforcement_agencies, beneficiary).

% Operate incarceration facilities that fill a substantial share of their bed-capacity with drug offense convictions. Lobby against decriminalization and sentencing reform. Collect per-diem payments from the state that scale directly with the criminalized population.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_operators, beneficiary,
    organized, biographical, arbitrage, national).

% Seize cash, vehicles, and property associated with alleged drug activity, often without conviction. Revenue from forfeiture funds departmental equipment and operations directly, creating a fiscal incentive tied to continued enforcement intensity rather than to public safety outcomes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, asset_forfeiture_units, beneficiary,
    institutional, biographical, arbitrage, national).

% Supply the black market that criminalization creates by eliminating legal competition. Prohibition removes quality control, price transparency, and dispute resolution from legitimate channels, concentrating supply-side profit in organizations willing to use violence to enforce contracts and territory.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, cartel_organizations, beneficiary,
    organized, generational, arbitrage, continental).

% Face arrest, prosecution, and incarceration for possession or use, often independent of any harm to third parties. Criminal records foreclose employment, housing, and family stability long after any sentence is served. Exit requires either cessation of use (not always medically or practically available on demand) or geographic flight, neither of which is reliably accessible.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_using_individuals, payer,
    powerless, biographical, trapped, local).

% Serve sentences for possession or low-level distribution offenses. Carry felony records that permanently restrict voting rights, professional licensure, and public housing eligibility in many jurisdictions — costs that persist well beyond the sentence itself and compound across a lifetime.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, convicted_nonviolent_offenders, payer,
    powerless, biographical, trapped, local).

% Bear enforcement intensity disproportionate to rates of substance use relative to other demographic groups, per longitudinal arrest and sentencing data. Experience compounding generational effects: incarcerated parents, disrupted household income, and diminished community trust in state institutions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, communities_of_color, payer,
    moderate, generational, constrained, national).

% Absorb the economic and caregiving costs of a household member's incarceration — lost income, legal fees, and disrupted childcare — without having used any substance themselves. Their situation is a direct externality of the state's chosen enforcement mechanism.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, biographical, trapped, local).

% Live in neighborhoods where prohibition-driven black markets settle disputes through violence rather than legal recourse. Bear injury, death, and property risk from a market structure created by the criminalization choice itself, despite having no role in the underlying transaction.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, bystanders_in_black_market_violence, payer,
    powerless, immediate, trapped, local).

% Political and religious constituencies whose worldview treats substance use as inherent moral failure. Receive symbolic vindication from the state's punitive stance, reinforcing group identity and political mobilization around 'law and order' platforms, independent of any measurable public-health outcome.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, moral_order_constituency, beneficiary,
    organized, generational, constrained, national).

% Produce evidence on comparative outcomes of criminalization versus treatment-based approaches but are structurally excluded from statute-setting processes dominated by law-enforcement and prosecutorial constituencies. Their findings are cited in academic and harm-reduction venues but rarely reach the legislative floor with comparable weight to enforcement-agency testimony.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_researchers, excluded,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, narcotics_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible signal of collective moral disapproval that some constituencies genuinely want expressed through law, and channels diffuse anxiety about substance-related social disorder into a single, visible state response.
% TRANSFER_FUNCTION: Moves liberty, income, and long-term life-opportunity from substance-using individuals and their families to enforcement agencies (budget, personnel, forfeiture revenue), incarceration operators (per-diem revenue), and illicit supply organizations (monopoly rents created by the absence of legal competition).
% ABSENT_VOICES: Public health researchers and formerly incarcerated individuals are rarely seated at the statute-drafting table; harm-reduction practitioners and drug policy reform advocates testify in hearings but structurally lose to enforcement-agency budget testimony and victim-of-crime narratives that dominate legislative attention.
% DISAPPEARANCE_RATIONALE: If criminal punishment for substance use vanished overnight, enforcement agency budgets tied to drug task forces would collapse, private prison populations would drop sharply, cartel revenue models built on prohibition-created price premiums would be disrupted, and millions of existing criminal records would need retroactive review — the reorganization would be immediate and structural, not cosmetic.
% FOUNDING_PROBLEM: Early 20th-century temperance and anti-narcotics movements framed substance use as a threat to social order, family stability, and racial/national purity narratives of the era, seeking a unified moral and legal response to perceived social decay.
% FOUNDING_PROBLEM_CORROBORATION: Enforcement agencies and moral-order constituencies attest the founding problem remains live, citing overdose deaths and visible public disorder as evidence social harm continues. Independent public health researchers, several government commissions (e.g., decades of federal drug policy review bodies), and international bodies such as the WHO and Global Commission on Drug Policy attest from outside the benefiting coalition that criminalization has failed to reduce use rates and has instead generated the enforcement and black-market harms now dominant — supporting a 'shifted function, original problem unsolved' reading.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because criminal liability attaches to the substance user directly as a moral-transgression target, not merely as a bystander to someone else's harm — the transfer runs from the user's liberty and life-opportunity to the enforcement apparatus's budget and the incarceration sector's revenue. Suppression is very high (0.88) because the constraint's persistence depends on continuous criminal enforcement machinery — arrest, prosecution, incarceration, and asset seizure — none of which would occur absent active state coercion. Theater ratio sits at 0.52 and rises over the interval because an increasing share of enforcement activity (task force operations, high-profile seizure announcements, mandatory minimum sentencing rhetoric) functions as political signaling of moral seriousness rather than measurable reduction in use rates or harm. Accessibility collapse is moderate (0.42), not near-mountain levels, because meaningful policy alternatives (decriminalization, harm reduction, regulated legalization) are visibly implemented in other jurisdictions and are actively debated — this is not a constraint where alternatives are unimaginable, only suppressed within this jurisdiction's political economy.
 *
 * PERSPECTIVAL GAP:
 *   From the enforcement agency's seat, this reading is a rope: it coordinates a shared social response to a genuine problem (substance-related harm) with participants (the public) as net beneficiaries of order and safety. From the substance-using individual's seat, the identical structure operates as a snare-adjacent tangled rope: coercive extraction of liberty and life-opportunity for an act that, under the sibling readings, generates no criminal jeopardy at all. The engine computes this divergence from the structural beneficiary/victim/enforcement data; the claimed_type of tangled_rope reflects the genuine (if contested) coordination function — public order signaling — coexisting with clear asymmetric extraction from a powerless, trapped victim class.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance-using individuals and their families sit at the full-target end: trapped exit, powerless, direct criminal liability for an act that under the sibling readings would not generate criminal jeopardy at all. Enforcement agencies, private prison operators, and asset forfeiture units sit at the full-beneficiary end: institutional or organized power, arbitrage-grade exit (they can lobby to preserve their funding stream regardless of outcome data), and direct revenue capture. Cartel organizations are a structurally interesting beneficiary: prohibition creates their entire business model by eliminating legal competition — they benefit from the constraint's existence without being named as its intended object at all, which is precisely the secondary externality the kernel's expected structural delta anticipates. Communities of color are declared as a payer group with moderate rather than powerless power atom to reflect organized political resistance capacity even while bearing disproportionate enforcement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (social decay attributed to substance use in early 20th-century moral-reform movements) is contested as still live: enforcement agencies point to overdose deaths as ongoing harm, while public health bodies and international commissions point to the criminalization apparatus itself as the primary driver of the harms now observed (overdose from unregulated supply, incarceration-driven poverty, black-market violence). This is the mandatrophy signature: an apparatus whose original justification (protecting social order) has been substantially undermined by evidence that the apparatus itself generates disorder (cartel violence, mass incarceration, intergenerational family disruption), while continuing to expand its footprint (rising suppression_requirement and theater_ratio over the interval) because its beneficiaries — enforcement agencies, incarceration operators, forfeiture units — have accumulated concentrated capacity to defend the mandate independent of its founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_transgression_vs_captured_apparatus,
    'Is the prohibition reading''s persistence driven by a genuine, still-live moral/social-order concern held by a broad constituency, or has the apparatus become self-perpetuating through the concentrated institutional interests (enforcement budgets, incarceration revenue, forfeiture funding) that now depend on its continuation regardless of the original concern''s status?',
    'Compare public opinion polling on substance-use moral framing over multiple decades against enforcement-agency budget growth and lobbying expenditure trends; if budget/lobbying growth substantially outpaces or diverges from underlying public moral concern, the apparatus-capture hypothesis is supported.',
    'If apparatus capture dominates, the tangled_rope classification is generous — the coordination function (public order) may be almost entirely displaced by the extraction function, pushing the structural type toward snare. If genuine moral concern remains the dominant driver, the tangled_rope classification with a real (if contested) coordination component is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_transgression_vs_captured_apparatus, empirical, 'Whether the prohibition apparatus is sustained by live moral consensus or by captured institutional interest.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the prohibition, harm_reduction, and legalization readings of the substance_control_kernel ever be adjudicated by a shared empirical standard, or do they rest on genuinely incommensurable premises about the proper role of state coercion in individual behavior?',
    'Track whether jurisdictions that have shifted between readings (e.g., decriminalization reversals, re-criminalization after legalization experiments) converge on outcome metrics (overdose rates, incarceration rates, black-market violence) that any reading''s proponents would accept as dispositive, or whether disputes persist even given shared data.',
    'If readings are empirically adjudicable, the kernel contest is temporary and should resolve toward whichever reading the evidence supports. If genuinely incommensurable (rooted in differing views of state authority''s proper scope, not just differing predictions), the three-reading structure is a stable, permanent feature of the policy landscape rather than a transitional disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel''s sibling readings are empirically resolvable or reflect a permanent normative fork.').

omega_variable(
    racial_disparity_intentionality,
    'Is the disproportionate enforcement burden on communities of color a designed feature of the prohibition reading''s implementation, or an emergent artifact of resource allocation and policing patterns independent of the statute''s text?',
    'Historical legislative record analysis (e.g., documented statements from statute drafters) combined with comparative enforcement-intensity data controlling for actual use-rate parity across demographic groups.',
    'If designed, the tangled_rope''s asymmetric extraction is more precisely targeted than the general ''users as a class'' framing suggests, and the victim declaration should be weighted toward communities of color specifically rather than substance users as an undifferentiated group. If emergent, the disparity is a downstream externality of otherwise race-neutral statute, which still counts as extraction but locates the causal mechanism differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_intentionality, empirical, 'Whether racially disparate enforcement is designed into or emergent from the prohibition apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(subs_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(subs_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(subs_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, asset_forfeiture_incentive_structure).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, mandatory_minimum_sentencing_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_kernel (prohibition_reading, harm_reduction_reading, legalization_reading), each authored as a separate ε-invariant constraint per the ε-invariance principle. The prohibition reading shows the highest extractiveness and suppression of the three because it alone places substance users directly in the criminal victim set; the sibling readings redirect state authority toward treatment provision or liberty-protection respectively, producing structurally different beneficiary/victim sets and lower coercive suppression. All three should be linked via affects_constraints to preserve the family structure for contamination and drift analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
