% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Operative Clause Bounds, Prefatory Clause Does Not Limit
 *   domain: constitutional/political/regulatory
 *
 * SUMMARY:
 *   The Second Amendment reads: 'A well regulated Militia, being necessary to
 *   the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' This constraint story instantiates the
 *   INDIVIDUAL-RIGHT READING: the operative clause ('the right of the people
 *   to keep and bear Arms') establishes a pre-existing individual right, and
 *   the prefatory militia clause ('A well regulated Militia, being
 *   necessary...') states a purpose or historical context but does not limit
 *   the right's scope. Under this reading, individuals possess a
 *   constitutionally protected claim to acquire and possess firearms for
 *   lawful purposes (including self-defense) that is not contingent on
 *   militia service or state authorization. State firearm regulations
 *   (licensing, background-check expansion, feature restrictions, capacity
 *   limits) are treated as presumptive infringements on this right, subject
 *   to heightened judicial scrutiny. This reading produces a tangled rope
 *   structure: it coordinates an interpretation of constitutional text
 *   (genuine coordination function) while simultaneously extracting from
 *   those harmed by unrestricted firearm access (victims of mass shooting,
 *   domestic violence, and firearm suicide). The constraint's extractiveness
 *   has risen steeply since 2008 (Heller decision), as courts have
 *   increasingly adopted and expanded the individual-right reading's scope.
 *
 * KEY AGENTS:
 *   - Firearms manufacturers and retailers: beneficiaries; protected from aggressive state regulation
 *   - Gun-rights advocates and litigation organizations: agenda-setters; control constitutional interpretation through test cases and briefing
 *   - Individual possession claimants: beneficiaries; claim a constitutional right to firearm acquisition and possession
 *   - Mass shooting survivors, domestic violence survivors, suicide-attempt survivors: victims/payers; bear costs of reduced regulatory capacity
 *   - Public-health and law-enforcement regulators: institutional payers; constrained regulatory mandate, shifted burden of proof
 *   - Courts (particularly US Supreme Court and federal appellate courts): agenda-setters; declare and enforce the reading through doctrine
 *   - Militia-reading and insurrectionist-reading advocates: excluded; unable to impose their interpretations on mainstream legal doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.71).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading: Operative Clause Bounds, Prefatory Clause Does Not Limit").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional/political/regulatory").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'c89ab5b3-b235-404c-be03-390fc91a0fcf').
narrative_ontology:cs_kernel_codification('c89ab5b3-b235-404c-be03-390fc91a0fcf', fixed_text).
narrative_ontology:cs_authority_grounding('c89ab5b3-b235-404c-be03-390fc91a0fcf', lineage).
narrative_ontology:cs_interpretation_layer_present('c89ab5b3-b235-404c-be03-390fc91a0fcf').
narrative_ontology:cs_reading_relation('c89ab5b3-b235-404c-be03-390fc91a0fcf', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('c89ab5b3-b235-404c-be03-390fc91a0fcf', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('c89ab5b3-b235-404c-be03-390fc91a0fcf', foundational, operative_clause_textual_primacy).
narrative_ontology:cs_axiom_status(operative_clause_textual_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c89ab5b3-b235-404c-be03-390fc91a0fcf', operative_clause_textual_primacy, empirically_contingent).
narrative_ontology:cs_axiom('c89ab5b3-b235-404c-be03-390fc91a0fcf', foundational, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('c89ab5b3-b235-404c-be03-390fc91a0fcf', prefatory_clause_non_limiting, empirically_contingent).
narrative_ontology:cs_reference_frame('c89ab5b3-b235-404c-be03-390fc91a0fcf', founding_individual_liberty_framework).
narrative_ontology:cs_drift_state('c89ab5b3-b235-404c-be03-390fc91a0fcf', contemporary_gun_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c89ab5b3-b235-404c-be03-390fc91a0fcf', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_possession_claimants).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_bereaved).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_safety_regulators).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, individual_natural_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, prefatory_clause_non_limiting_principle).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, operative_clause_textual_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the constitutional protections shield firearms from aggressive state regulation, limiting manufacturers' legal liability and expanding their addressable market. They benefit from any constitutional ruling that treats firearm regulation as presumptive infringement. Their primary advantage is that heightened scrutiny becomes the default for new firearms restrictions, requiring state justification rather than industry compliance. They do not bear the direct costs of gun violence but profit from the market conditions this reading enables.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers, beneficiary,
    institutional, generational, arbitrage, national).

% Operate within a constitutionally-protected market for firearms sales. This reading restricts states' ability to impose licensing, background-check expansion, waiting periods, or other transaction regulations that increase their operational cost. They collect the benefit of a stable, protected sales channel with presumptively unconstitutional regulatory barriers.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_retailers, beneficiary,
    organized, biographical, mobile, national).

% Set the framing of the right through litigation strategy, legislative testimony, and public discourse. They establish the reading's canonical interpretation by selecting test cases, briefing doctrine, and mobilizing political support. They benefit directly from each state court and federal court ruling adopting the individual-right reading, and from legislative victories that codify the interpretation. They function as the agenda-setter because they design the argument, control which constraints reach courts, and determine how courts frame the operative/prefatory distinction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, gun_rights_advocates, beneficiary).

% Under this reading, possess a constitutionally-grounded individual right to acquire and keep firearms for personal self-defense. They benefit from the interpretation because state restrictions on their possession, feature regulations on the firearms they can own, or licensing regimes they must navigate are constitutionally suspect. Their situation is bounded by the reading: the right is theirs as individuals, not contingent on militia service or state designation. State regulation that was permissible under the militia-conditioned reading becomes presumptively unconstitutional under this one.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_possession_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Bear the costs of this reading in the form of reduced regulatory barriers to firearm acquisition by individuals who commit mass shooting attacks. This reading constrains states' ability to adopt comprehensive background-check regimes, extreme-risk-protection orders, or firearm-capacity restrictions that might prevent some shooting events. They are harmed by the constraint's operation (by foreclosing regulations that might have prevented their harm), yet structurally powerless to exit the polity or contest the constitutional interpretation directly. Their testimony appears in legislative hearings and litigation, but the institutional power to interpret the constraint lies with courts aligned with the reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_survivors, payer,
    powerless, biographical, trapped, national).

% Bear costs from this reading when state efforts to restrict firearm access by domestic-violence perpetrators are struck down as unconstitutional. The individual-right reading treats domestic-violence firearm restrictions as presumptive infringements, shifting the burden to the state to justify why a convicted abuser loses the right. Survivors' access to protective remedies (temporary firearm removal, surrender-on-conviction) contracts when courts adopt the individual-right framing. They cannot exit this regulatory domain and depend on state legislative workarounds that courts may strike down.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_survivors, payer,
    powerless, biographical, trapped, national).

% Under this reading, states face constitutional constraints on firearm-removal policies (temporary surrender during crisis, safe-storage mandates) that epidemiological evidence links to suicide-attempt prevention. The individual-right reading treats access restrictions as presumptive infringements, even for evidence-based suicide-prevention interventions. The bereaved cannot reverse the constraint or escape the jurisdiction, and state innovations in crisis-removal infrastructure are legally vulnerable.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_bereaved, payer,
    powerless, biographical, trapped, national).

% State and local public-health and law-enforcement officials operate under a constrained regulatory mandate when the individual-right reading applies. Their authority to regulate firearms in response to epidemiological evidence, community violence, or public-health crises becomes subject to heightened constitutional scrutiny. They bear the cost of defending regulations they believe evidence-based and necessary; regulations are struck down or never attempted because of constitutional vulnerability. They function as institutional payers because their jurisdiction must absorb the violence burden (emergency response, medical costs, community impact) while their regulatory tools contract.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_safety_regulators, payer,
    institutional, generational, constrained, national).

% Federal and state courts adjudicate disputes over the amendment's meaning and enforce the individual-right reading through doctrine. They set the canonical interpretation through flagship opinions (DC v. Heller as the touchstone for this reading at the federal level), define the scope of permissible regulation, and determine how much states must justify their firearms laws. Courts function as agenda-setters because they declare what the Constitution means and enforce the reading through judicial review of state and federal regulations.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, courts_adopting_reading, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that the prefatory clause binds the operative clause's scope to collective defense contexts, permitting comprehensive state regulation of private firearms. They are excluded from control of constitutional interpretation by courts and litigation strategy; their preferred reading is not currently the dominant one in federal jurisprudence (though it retains support in some state courts and academic discourse). They cannot force the interpretation they prefer and must work through the same institutional channels (litigation, legislation, appointment of sympathetic judges) that the individual-right advocates control.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, militia_reading_advocates, excluded,
    organized, generational, trapped, national).

% Contend that the right exists to preserve armed-resistance capacity against tyrannical government and emphasize the revolutionary-context founding. This reading is even further from mainstream constitutional doctrine than the militia reading; insurrectionist advocates are excluded from official legal interpretation and face pressure from federal law-enforcement when they act on the insurrectionist premise. They remain trapped within the polity they contest, unable to shift the legal interpretation toward their preferred reading through mainstream institutional channels.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, insurrectionist_reading_advocates, excluded,
    organized, generational, trapped, national).

% Analytical seat: historians and constitutional scholars who evaluate what the operative and prefatory clauses meant at ratification and assess which modern reading aligns with original public meaning. They provide evidentiary substrate for judicial opinions and academic debate. They are not beneficiaries or payers of the constraint; they function as observers who help adjudicate between competing readings through historical evidence and textual analysis.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, original_public_meaning_interpretivists, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the allocation of individual-level firearm-access rights independent of state-collective-defense assessment. The reading coordinates an interpretation of the Constitution such that individuals have a claim to firearm possession that does not depend on their militia status or service. Under this reading, the right is not a coordination mechanism for collective defense (which would be the militia reading's function) but rather an individual liberty that the state must justify restricting—a coordination of the interpretation of constitutional text.
% TRANSFER_FUNCTION: Transfers regulatory authority from states (the ability to regulate private firearms as public health matters) to the federal constitution (which treats firearm restrictions as presumptive infringements requiring heightened judicial scrutiny). This shifts the burden of proof: states must now justify restrictions rather than individuals justifying their need to possess firearms. The transfer also moves political power toward organized gun-rights advocates and firearm manufacturers (who gain from constrained regulation) and away from public-health regulators (who lose regulatory tools). Firearm-access harm costs (mass shooting, domestic violence, suicide) are borne by victims and survivors, while manufacturers and retailers internalize regulatory burden reduction.
% ABSENT_VOICES: The militia-reading advocates and insurrectionist-reading advocates would argue for a different interpretation of the amendment's text and purpose. They are excluded from controlling the dominant constitutional interpretation through institutional channels (they can litigate and legislate, but courts currently align with the individual-right reading). Victims of firearms violence—mass shooting survivors, domestic violence survivors, suicide-attempt survivors—would argue that the reading privileges an abstract constitutional right over the empirical harms of unrestricted firearm access; they are systematically excluded from courts' decisions about the right's scope (their testimony is heard in some legislative contexts but does not constrain judicial interpretation). Epidemiologists and public-health researchers have limited standing in constitutional discourse and are excluded from courts' definition of what counts as a compelling state interest sufficient to override the right.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment disappeared—if courts reverted to the militia-conditioned reading or adopted a reading that treated prefatory and operative clauses as inseparable—the regulatory landscape would reorganize dramatically. States could adopt comprehensive firearm-licensing regimes, background-check expansion, extreme-risk-protection orders, and capacity/feature restrictions without constitutional vulnerability. The firearm industry would face new regulatory burdens, gun-rights advocacy would lose its current doctrinal anchor, and individuals claiming a personal right to possess firearms would have to frame their claims as legislative policy preferences rather than constitutional entitlements. Public-health regulators would gain back the tools they currently lack. The removal of this reading does not affect the text of the Second Amendment itself, but it removes the interpretive authority that currently shields firearms from state regulation—the world of regulations, litigation strategy, and political power re-aligns entirely.
% FOUNDING_PROBLEM: At the Founding, the concern animating Second Amendment advocates was preservation of individual capacity to resist tyrannical government and participate in collective defense without permanent militia structures. The operative clause (the right to keep and bear arms) was treated as expressing a pre-existing right (grounded in natural law or common-law tradition), and the prefatory clause (a well-regulated militia being necessary) was understood as stating a purpose or context, not a limiting condition. The individual-right reading recovers this original framing: the right is individual, pre-existing, and not conditioned on militia service or state authorization.
% FOUNDING_PROBLEM_CORROBORATION: Originalist constitutional scholars and historians including Randy Barnett, Eugene Volokh, and Ilya Somin argue from primary sources (founding-era legal texts, state ratification debates, contemporaneous political writing) that the individual-right reading matches the founding understanding. The DC v. Heller majority opinion (2008) relies on this historical scholarship to establish the individual-right reading as aligned with original public meaning. However, this corroboration comes from scholars and judges who favor the individual-right reading and benefit from its adoption. Counter-evidence from historians like Saul Cornell and Michael Waldman, who argue that founding-era sources do not support an unfettered individual right and that prefatory-clause conditioning was well-understood at ratification, offers competing corroboration outside the individual-right camp. The empirical historical record is genuinely contested; no external, neutral arbiter has settled whether original public meaning supports the individual-right or militia-conditioned reading.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the individual-right reading protects the firearms industry from state regulation and shifts regulatory burden to states (they must justify restrictions rather than industry justifying sales). This extraction is not traditional rent-seeking (no single party collects a transfer payment) but rather extraction in the form of regulatory forbearance: manufacturers and retailers benefit from the constraint's operation (reduced compliance costs, expanded market), while victims benefit from the opposite (a reading where the prefatory clause limits the operative clause). The measurement series shows rising extractiveness from 1791 (when the right was rarely litigated) through 2024, with the steepest increase after 2008 (Heller), when federal courts adopted the individual-right reading and began striking down state regulations. Suppression is high (0.71) because the constraint's persistence depends on courts actively enforcing the individual-right reading against state regulatory efforts—without judicial enforcement, states could return to permissive regulation. Theater ratio (0.42) reflects the increasing rhetorical emphasis on the right as a foundational individual liberty (heightened theater) while actual regulatory constraints remain contested (unresolved scope disputes mean not all states apply the reading identically).
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer and gun-rights seat, the constraint is rope: genuine coordination on constitutional meaning. From the victim and regulator seat, the constraint is snare: a cover story (constitutional interpretation) that masks extraction (regulatory forbearance benefiting a dangerous industry while preventing harm reduction). Courts perceive the constraint as mountain: the reading is the Constitution's true meaning, not a constructed choice. These divergences are not measurement error but structural divergences: different seats literally see different constraints because they have different epistemic access to 'what the right means' and different stakes in its scope.
 *
 * DIRECTIONALITY LOGIC:
 *   The individual-right reading benefits those who profit from unrestricted firearm access (manufacturers, advocates) and burdens those harmed by it (shooting survivors, regulators). The structural asymmetry is not accidental: advocates have organized to place sympathetic judges who adopt their reading, while victims are fragmented and excluded from constitutional adjudication. The reading's persistence depends on continued judicial enforcement against state regulatory efforts (suppression = 0.71), meaning the constraint is not natural or inevitable but actively maintained by courts aligned with the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not classified as a piton (degraded/inertial function). The individual-right reading has substantial structural support: organized gun-rights advocates invest heavily in litigation, judges actively apply the reading, and beneficiary seats (manufacturers, retailers) have concentrated interests in maintaining it. The founding problem (preservation of individual capacity to resist tyranny) is contested but not dead—gun-rights advocates argue it remains live as a check on government overreach, while regulators argue the problem was solved at the Founding and subsequent regulations serve legitimate public-health interests. The vanishing-problem test suggests mandatrophy may be emerging: if the individual-right reading was justified by the danger of tyranny or militia necessity, and if modern firearm-rights advocates do not frame the right primarily as militia-related but as personal self-defense, then the founding justification (militia necessity) has drifted away from the right's modern operation (personal possession for lawful purposes). However, gun-rights advocates have reframed the founding problem to emphasize tyranny-prevention and individual liberty (not militia necessity), which keeps the right's justification alive, even if transformed. This is not mandatrophy (where the arrangement persists but no party can explain why) but rather re-justification: advocates have made a strong case for the founding problem's ongoing relevance, just with a different emphasis than the historical text suggests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_interpretation_ambiguity,
    'What did the operative and prefatory clauses mean at ratification: does the prefatory clause condition the operative clause''s scope, or does it merely state a purpose without limiting the right?',
    'Systematic analysis of founding-era legal and political sources (state ratification debates, constitutional convention records, founding-era legal treatises) using modern historical methodology; comparison with how other founding-era documents used parallel prefatory and operative structures.',
    'If the prefatory clause was understood as conditioning the right to militia contexts, the militia-conditioned reading is vindicated and states retain broad regulatory authority. If the prefatory clause was understood as non-limiting, the individual-right reading''s historical premise is strengthened. This ambiguity is the root epistemic uncertainty underlying the entire kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_interpretation_ambiguity, empirical, 'Whether the prefatory clause limits the operative clause''s scope at ratification.').

omega_variable(
    kernel_reading_distinction_ambiguity,
    'Is this constraint a reading of a contested kernel (the Second Amendment''s meaning), or is it an ordinary constraint about firearm regulation?',
    'Declare the reading explicitly and distinguish it from sibling readings through the cs_structure fields. A reading is one instantiation of a kernel; siblings are other instantiations. All are valid constraint stories; the committer structure routes through omegas rather than standard fields.',
    'This omega documents that the constraint''s classification depends on accepting the individual-right reading as the correct interpretation of the kernel. Under the militia-conditioned reading, the constraint would have a different ε (lower extractiveness because state regulation would be constitutionally permissible), different beneficiaries/victims, and likely a different type (rope or mountain from different seats). The committer frame acknowledges this reading-dependence without resolving the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction_ambiguity, conceptual, 'Recognition that this constraint is one reading of the Second Amendment kernel, not a neutral description of firearm regulation.').

omega_variable(
    individual_right_scope_ambiguity,
    'If the operative clause establishes an individual right, what is the scope of that right: does it protect all firearm acquisition and possession without limit, or does it permit some state regulation as ''reasonable''?',
    'Court doctrine on what counts as presumptive infringement vs. permissible regulation (currently settled by Heller''s intermediate-scrutiny framework at the federal level, but contested in state courts and academic debate). Empirical test: do jurisdictions adopting the individual-right reading implement widely divergent firearms regulations, or do they converge on a common scope?',
    'The scope of the individual right directly determines the constraint''s extractiveness. A broad-scope reading (almost all regulation is presumptive infringement) produces high ε and high suppression; a narrow-scope reading (many regulations are permissible) produces lower ε. This ambiguity explains variation in how different courts apply the individual-right reading and affects the constraint''s type across jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_right_scope_ambiguity, empirical, 'The scope of the individual right once established determines permissible state regulation.').

omega_variable(
    empirical_harms_vs_constitutional_right,
    'How should empirical evidence about firearm-harm causation (mass shooting prevention through background checks, suicide reduction through access restrictions, domestic violence lethality increase with firearm access) weigh against a constitutionally-protected right?',
    'Constitutional courts'' treatment of empirical evidence in heightened-scrutiny review of firearm regulations. Empirical question: do harm-reduction regulations that are struck down as unconstitutional actually prevent the harms they target? Normative question: should constitutional rights be constrained by empirical evidence of harm, or is that an illegitimate restriction on rights?',
    'If courts increasingly weigh empirical evidence heavily, some regulations may survive heightened scrutiny (narrowing the individual right''s effective scope). If courts treat rights as largely immune to harm evidence, the right remains broadly protected and victims'' interests remain structurally excluded from constitutional adjudication. This affects the constraint''s theater_ratio (the performative portion of rights rhetoric vs. actual regulatory constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_harms_vs_constitutional_right, preference, 'Whether constitutional rights should be constrained by empirical evidence of the harms they enable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_boundary__individual_right_reading, theater_ratio, 1791, 0.08).
narrative_ontology:measurement_basis(seco_tr_t1791, projected).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_boundary__individual_right_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1868, projected).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_boundary__individual_right_reading, theater_ratio, 1934, 0.18).
narrative_ontology:measurement_basis(seco_tr_t1934, observed).
narrative_ontology:measurement(seco_tr_t1976, second_amendment_boundary__individual_right_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement_basis(seco_tr_t1976, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__individual_right_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_boundary__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, projected).
narrative_ontology:measurement(seco_be_t1868, second_amendment_boundary__individual_right_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement_basis(seco_be_t1868, projected).
narrative_ontology:measurement(seco_be_t1934, second_amendment_boundary__individual_right_reading, base_extractiveness, 1934, 0.18).
narrative_ontology:measurement_basis(seco_be_t1934, observed).
narrative_ontology:measurement(seco_be_t1976, second_amendment_boundary__individual_right_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement_basis(seco_be_t1976, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_boundary__individual_right_reading, suppression_requirement, 1791, 0.25).
narrative_ontology:measurement_basis(seco_su_t1791, projected).
narrative_ontology:measurement(seco_su_t1868, second_amendment_boundary__individual_right_reading, suppression_requirement, 1868, 0.45).
narrative_ontology:measurement_basis(seco_su_t1868, observed).
narrative_ontology:measurement(seco_su_t1934, second_amendment_boundary__individual_right_reading, suppression_requirement, 1934, 0.52).
narrative_ontology:measurement_basis(seco_su_t1934, observed).
narrative_ontology:measurement(seco_su_t1976, second_amendment_boundary__individual_right_reading, suppression_requirement, 1976, 0.58).
narrative_ontology:measurement_basis(seco_su_t1976, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.15).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearm_regulation_state_authority).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, mass_shooting_prevention_policy_space).

% DUAL FORMULATION NOTE:
% The second_amendment_boundary kernel decomposes into three constraint stories: individual-right reading (this file), militia-conditioned reading, and insurrectionist reading. Each reading instantiates a different constraint with a different ε, different beneficiaries/victims, and different computed type. The three are not observables of a single constraint but rather distinct constraints that share a common kernel text. They are linked through network.affects_constraints to show the constraint-family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, powerless, 0.95).
constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
