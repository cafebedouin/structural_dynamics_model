% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment reads: 'A well regulated Militia, being necessary to
 *   the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' The individual-right reading interprets
 *   the operative clause ('the right of the people to keep and bear Arms,
 *   shall not be infringed') as establishing a pre-existing, unenumerated
 *   individual right to possess firearms. The prefatory militia clause is
 *   read as stating a purpose or justification but not limiting the right's
 *   scope — private possession for self-defense, hunting, and sport falls
 *   within protection. This reading, established in District of Columbia v.
 *   Heller (2008) and expanded in McDonald v. City of Chicago (2010) and New
 *   York State Rifle & Pistol Association v. Bruen (2022), treats firearms
 *   regulation as presumptively unconstitutional unless it survives strict
 *   scrutiny or falls within narrow historical exceptions. The constraint
 *   structures state regulatory authority, judicial review, and the market
 *   for firearms. Beneficiaries are manufacturers, gun-rights advocates, and
 *   gun owners. Victims are those harmed by unrestricted access: mass
 *   shooting victims, domestic violence survivors, and those at risk of
 *   firearm suicide. The structure is Tangled Rope: a genuine coordination
 *   function (stable constitutional frame, reduced judicial uncertainty)
 *   rides atop asymmetric extraction (regulatory authority transferred from
 *   legislatures to courts; vulnerability concentrated in harm-bearing
 *   groups).
 *
 * KEY AGENTS:
 *   - Supreme Court originalist coalition: agenda-setter, institutional power, sets the authoritative reading through binding doctrine, controls interpretation of scope and permissible regulation
 *   - Firearm manufacturers: beneficiary, powerful, market access protected from broad regulation, constitutionally shielded product category
 *   - Gun-rights advocacy organizations: beneficiary, organized, institutional legitimacy from constitutional framing, mobilization and litigation power
 *   - Individual gun owners: beneficiary + payer (dual), organized, gain constitutional protection but also bear diffuse costs of regulatory uncertainty and polarization
 *   - Mass shooting victims and families: victim/payer, powerless, excluded from constitutional reading process, face narrowed regulatory remedies, harm uncompensated
 *   - Domestic violence survivors: victim/payer, powerless, face heightened lethality, regulatory interventions (extreme-risk orders, abuser disqualification) face constitutional challenge
 *   - Suicide-attempt survivors: victim/payer, powerless, means-restriction approaches (waiting periods, secure storage, crisis removal) face constitutional uncertainty
 *   - Gun violence researchers: victim/payer (secondary), moderate power, research marginalized in constitutional reasoning because right does not require empirical validation
 *   - State legislatures (pro-regulation): payer, institutional, regulatory reach constrained, legislative authority subordinated to judicial constitutional review, must carry litigation burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '910b172c-22d2-4b67-ae60-fa9e19bc9318').
narrative_ontology:cs_kernel_codification('910b172c-22d2-4b67-ae60-fa9e19bc9318', fixed_text).
narrative_ontology:cs_authority_grounding('910b172c-22d2-4b67-ae60-fa9e19bc9318', lineage).
narrative_ontology:cs_interpretation_layer_present('910b172c-22d2-4b67-ae60-fa9e19bc9318').
narrative_ontology:cs_reading_relation('910b172c-22d2-4b67-ae60-fa9e19bc9318', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('910b172c-22d2-4b67-ae60-fa9e19bc9318', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('910b172c-22d2-4b67-ae60-fa9e19bc9318', foundational, individual_rights_predate_government).
narrative_ontology:cs_axiom_status(individual_rights_predate_government, holdable).
narrative_ontology:cs_axiom_grounding('910b172c-22d2-4b67-ae60-fa9e19bc9318', individual_rights_predate_government, deontological).
narrative_ontology:cs_axiom('910b172c-22d2-4b67-ae60-fa9e19bc9318', foundational, prefatory_clauses_do_not_limit_operative_text).
narrative_ontology:cs_axiom_status(prefatory_clauses_do_not_limit_operative_text, holdable).
narrative_ontology:cs_axiom_grounding('910b172c-22d2-4b67-ae60-fa9e19bc9318', prefatory_clauses_do_not_limit_operative_text, empirically_contingent).
narrative_ontology:cs_axiom('910b172c-22d2-4b67-ae60-fa9e19bc9318', secondary, right_not_empirically_gated_by_harm).
narrative_ontology:cs_axiom_status(right_not_empirically_gated_by_harm, holdable).
narrative_ontology:cs_axiom_grounding('910b172c-22d2-4b67-ae60-fa9e19bc9318', right_not_empirically_gated_by_harm, deontological).
narrative_ontology:cs_reference_frame('910b172c-22d2-4b67-ae60-fa9e19bc9318', individual_liberty_predate_government).
narrative_ontology:cs_drift_state('910b172c-22d2-4b67-ae60-fa9e19bc9318', post_heller_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('910b172c-22d2-4b67-ae60-fa9e19bc9318', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearm_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims_and_families).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_attempt_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, gun_violence_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_legislatures_pro_regulation).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, urban_communities_with_high_gun_violence_rates).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, individual_rights_predate_government).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, prefatory_clauses_do_not_limit_operative_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Second Amendment as protecting an individual pre-existing right to keep and bear arms. Sets the authoritative reading through constitutional doctrine. Issues opinions that define scope of regulation and establish presumptions of unconstitutionality for certain firearm restrictions. Controls the doctrine's interpretive frame.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, supreme_court_originalist_coalition, agenda_setter,
    institutional, generational, analytical, national).

% Operate in a constitutionally shielded market where broad regulatory approaches (bans, licensing, ammunition control) face legal challenge under the individual-right reading. Their business model depends on access to civilian consumers and on defeating regulations that would reduce that access. Market expansion and legal immunity from certain liability classes constitute the benefit.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Frame the constraint as protection of fundamental constitutional rights. Mobilize members to oppose regulations, fund litigation, and elect candidates who will appoint judges committed to the individual-right reading. Derive organizational power and funding streams from the reading's ongoing legitimacy and from active defense against regulatory challenges.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Gain constitutional protection for firearm possession, raising the burden on government to regulate access. Benefit from market availability of wide variety of firearms. Also bear diffuse costs: live in a regulatory environment shaped by litigation and constitutional uncertainty rather than clear public-health policy, face high litigation costs if enforcement attempts are made against them, and navigate the social polarization the reading generates.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, individual_gun_owners, payer).

% Bear the cost of mass shooting incidents. Their advocacy for broader regulations (assault weapon bans, universal background checks, waiting periods) encounters constitutional barriers erected by the individual-right reading. Legislative remedies they seek are challenged as unconstitutional; their harm is uncompensated and regulatory options to prevent recurrence are constrained.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims_and_families, payer,
    powerless, immediate, trapped, local).

% Face heightened lethality when domestic abuse partners have firearm access. Regulations targeting domestic abusers (extreme risk protection orders, mandatory firearm surrender in abuse cases, expanded disqualifier definitions) face constitutional challenges under the individual-right reading. Their escape options are constrained by the regulatory framework's constitutional exposure.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_survivors, payer,
    powerless, biographical, trapped, local).

% Benefit from means-restriction approaches (waiting periods, secure storage mandates, firearm removal during crisis intervention) but these face constitutional challenges under the individual-right reading. The constraint reduces the scope of public-health interventions targeting suicide-by-firearm, the most lethal and least reversible method.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, suicide_attempt_survivors, payer,
    powerless, biographical, trapped, local).

% Conduct epidemiological and statistical research on gun violence patterns, correlations between access restrictions and outcomes. Their work is marginalized in constitutional debates because the individual-right reading does not require empirical validation of claimed harms — the right is pre-existing and unenumerated. Their research is cited selectively in legislative debates but cannot constrain the constitutional floor.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_violence_researchers, payer,
    moderate, generational, constrained, national).

% Attempt to craft gun regulations reflecting their constituencies' preferences (safer schools, safer domestic environments, reduced suicide access). Their legislative authority to set firearms policy is constrained by the individual-right reading, which presumes many regulations unconstitutional and requires them to carry litigation burden and constitutional uncertainty. Their regulatory reach is narrower than the reading permits for other domains.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_legislatures_pro_regulation, payer,
    institutional, generational, constrained, national).

% Bear concentrated gun violence burden: homicide, suicide, accidental injury. Their elected representatives cannot easily implement the aggressive enforcement or market-restriction approaches (licensing systems, mandatory buybacks, ammunition taxation) that epidemiological evidence associates with reduced rates, because the individual-right reading treats such approaches as presumptively unconstitutional.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, urban_communities_with_high_gun_violence_rates, payer,
    powerless, immediate, trapped, local).

% Hold that the prefatory militia clause limits the operative right to collective-defense contexts. Their reading is structurally excluded from the current Supreme Court doctrine and would require reinterpretation of binding precedent. They are present in dissenting opinions and in the legislative/public debate but lack institutional power to set the constitutional frame.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, militia_reading_proponents, excluded,
    institutional, generational, trapped, national).

% Conduct academic analysis of the individual-right reading's textual, historical, and policy foundations. Challenge the originalist methodology and argue for alternative interpretive frames (living constitutionalism, purposivism). Their analysis influences lower courts, legislative deliberation, and public understanding but has not shifted the Supreme Court's binding doctrine.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, constitutional_scholars_critical_of_originalism, observer,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearm_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable constitutional frame for resolving disputes about firearm regulation: sets boundaries on permissible state action, reduces uncertainty for manufacturers and consumers about what regulations will survive constitutional scrutiny, provides a common language for judicial review across circuits.
% TRANSFER_FUNCTION: Transfers regulatory authority from elected representatives to the judiciary; transfers de facto veto power over certain classes of regulation to those who can mount constitutional challenges; transfers market-expansion rights to firearm manufacturers; transfers vulnerability and harm burden to victims of firearm violence whose regulatory options are constrained.
% ABSENT_VOICES: Those harmed by unrestricted firearm access (mass shooting victims, domestic abuse survivors, suicide-attempt survivors) are present in the constraint story but structurally excluded from setting the constitutional reading itself. Their harm would be heard if constitutional interpretation rested on empirical validation of regulatory necessity, but the individual-right reading does not require such validation. Alternative constitutional readings (militia-conditioned, harm-focused) would admit their harm narratives as central to scope definition; this reading marginalizes them as policy questions, not constitutional ones.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight and were replaced by the militia-conditioned reading, state and federal regulation of firearms would expand within months: assault weapon restrictions, universal background checks, licensing systems, ammunition taxation, and extreme-risk protection orders would face no constitutional barrier. Firearm manufacturers would face reduced market access and new liability exposure. Gun violence epidemiology would re-enter legislative reasoning as an authoritative input. The regulatory landscape would reorganize toward public-health models similar to tobacco, alcohol, and driving.
% FOUNDING_PROBLEM: Protection of an individual's capacity to possess firearms for lawful purposes (self-defense, hunting, sport) against claims that the Second Amendment permits comprehensive disarmament or near-total confiscation.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court originalist coalition attests the problem was live and unresolved before District of Columbia v. Heller (2008), citing historical regulations (selective disarmament of disfavored groups) as evidence of ongoing risk. Gun-rights advocates attest the problem remains live, citing proposed regulations they characterize as de facto bans. Gun-violence researchers and public-health authorities attest the founding problem is substantially solved in the regulatory context — no major jurisdiction proposes total disarmament — and the reading now persists as a constraint on evidence-based policy without corresponding protection function. State legislatures report that regulatory uncertainty rather than risk of confiscation shapes their behavior.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68 at interval end) is moderately high because the reading creates asymmetric authority: manufacturers and gun-rights advocates capture regulatory veto and market immunity, while harm-bearing populations have constrained policy options. The constraint is not pure extraction because it does solve a real coordination problem (constitutional stability reduces uncertainty for all parties, provides predictable judicial review standards). The coordination is asymmetric — beneficiaries get both coordination benefit AND extraction, while payers get extraction without coordination benefit. Suppression is moderate (0.42) because the constraint operates through law and adjudication rather than direct coercion; resistance is high (0.73) because the reading faces sustained opposition from harm-bearing constituencies, public-health advocates, and legislatures, with periodic mass mobilization after shooting incidents. Theater ratio (0.28) is moderate-low because the coordination function (predictable constitutional frame) is genuine, but it increasingly functions as cover for market protection rather than as the primary justification offered by beneficiaries. The measurement series show a rising trajectory in extractiveness and theater through the first decade after Heller, then plateau — indicating that the reading's extractive structure became locked in once the binding doctrine was established. Suppression requirement rises modestly (the reading requires less active suppression to maintain as it becomes normalized into institutional practice) and plateaus similarly. The plateau pattern is consistent with a Tangled Rope that has achieved institutional stability: the coordination is settled (low theater increase), the asymmetry is absorbed, and continued enforcement is maintenance rather than founding.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Supreme Court) and beneficiaries (manufacturers, advocates, gun owners) perceive the constraint as protecting fundamental individual liberty and solving the coordination problem of constitutional certainty. From these seats, the constraint is legitimate exercise of judicial power to protect rights against majoritarian erosion. The payer seats (mass shooting victims, domestic abuse survivors, researchers, state legislatures) perceive the same constraint as judicial usurpation of regulatory authority that should rest with elected representatives and as protecting market access at the cost of harm to vulnerable populations. The reading creates a structural asymmetry: the coordination benefit (stable constitutional frame) accrues evenly to all parties, but the extraction (market veto, regulatory constraint) flows only to beneficiaries. Payers receive extraction without coordination benefit — they could achieve constitutional stability through an alternative reading (militia-conditioned) that would provide coordination without extraction, but are excluded from that choice by the binding precedent the agenda-setter controls. The engine computes this divergence from power atoms, exit options, and beneficiary/victim declarations — beneficiaries have high exit (arbitrage, mobile) while payers have low exit (trapped, identity_locked for those defending the reading as constitutional truth).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: firearm manufacturers (institutional power, arbitrage exit = d near 0.0, full beneficiary); gun-rights organizations (organized power, mobile exit = d ~0.15); individual gun owners (organized power, constrained exit due to dual position as beneficiary and bearer of diffuse costs = d ~0.35, slightly net-beneficiary). Payers: mass shooting victims and families (powerless, trapped = d near 1.0, full target); domestic violence survivors (powerless, trapped = d near 1.0, full target); suicide-attempt survivors (powerless, trapped = d near 1.0, full target); gun violence researchers (moderate power, constrained exit = d ~0.75, strong target); state legislatures (institutional power but constrained by constitutional constraint on their authority = d ~0.55, moderate target). The Supreme Court agenda-setter sits outside the beneficiary/victim frame — it administers the constraint but does not collect from it (institutional, analytical exit = d ~0.5, symmetric). The directionality profile shows extreme asymmetry: beneficiaries cluster near d=0.0-0.35 (low extraction from them), payers cluster near d=0.75-1.0 (high extraction from them), with no density in the middle. This asymmetry is the structural signature of Tangled Rope with pronounced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection of individual firearm possession against comprehensive disarmament or near-total confiscation) has substantially been solved: no major jurisdiction proposes complete gun bans or confiscation, and the individual-right reading ensures that such proposals would face constitutional barrier. However, the constraint persists and has expanded its scope from 2008 to 2022. The expansion is documented in the measurement series: extractiveness rises from 0.52 to 0.68 as the reading's application broadens to strike down regulations that survive reasonable-fit scrutiny under older doctrine (assault weapon bans, permit requirements, age restrictions, ammunition controls). The constraint thus exhibits mandatrophy: the founding problem is dead (disarmament risk is negligible), but the constraint has not dissolved. Instead, it has been repurposed: from protecting against confiscation toward protecting market access and manufacturers' product diversity. This is detected in the theater-ratio plateau and the asymmetric beneficiary/victim structure — the coordination function is real but no longer the primary driver of the constraint's operation. The constraint persists because beneficiaries have institutional power to defend it and payers lack the institutional power to exit via alternative constitutional reading (they are locked into the binding precedent). The mandatrophy is partially acknowledged in public discourse (critics argue the reading protects market access rather than legitimate individual liberty) and partially denied (advocates assert the protection against regulation remains vital, pointing to periodic legislative proposals), so it should be classified as contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_scope_ambiguity,
    'Was the founding problem protection against near-total disarmament of the civilian population, or protection of a specific category of personal weapons (long arms, handguns, military-pattern rifles), or something broader?',
    'Historical research on founding-era militia composition, civilian firearm ownership patterns, and the scope of existing colonial and state regulations that the Framers knew. Originalist methodology itself generates evidence for which specific regulations the founding problem was responding to.',
    'A narrow founding problem (protection of long-arm ownership for militia-eligible men) would support narrower contemporary scope; a broad founding problem (protection against any meaningful civilian disarmament across weapon categories) would support the expansive contemporary reading. Current Supreme Court doctrine assumes broad scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_scope_ambiguity, empirical, 'Ambiguity in the scope of the founding problem the individual-right reading claims to address.').

omega_variable(
    prefatory_clause_semantic_function,
    'Does the prefatory militia clause limit the scope of ''the right of the people to keep and bear Arms,'' or does it merely state a purpose without limiting scope?',
    'Historical and linguistic analysis of 18th-century drafting conventions, comparative study of how other prefatory clauses functioned in founding-era documents, originalist reinterpretation of the text''s grammar and context.',
    'If the clause limits scope, the militia-conditioned reading would foreclose the individual-right reading entirely — the operative clause would apply only in militia context. If the clause does not limit scope, the individual-right reading stands. This is the central axis of contest between the two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_semantic_function, conceptual, 'The semantic and structural relationship between the prefatory and operative clauses — whether limitation or mere purpose-statement.').

omega_variable(
    regulatory_necessity_empirical_gate,
    'Does the individual-right reading require empirical validation of regulatory necessity (as other constitutional tests do), or does the pre-existence of the right foreclose empirical gating?',
    'Supreme Court clarification through future opinions on whether public-health evidence about specific regulations'' effects (e.g., waiting periods and suicide prevention, universal background checks and criminal access) can satisfy constitutional review. Current doctrine suggests that the right is not empirically gated, but this remains contested at the margins.',
    'If empirical necessity gates the right, gun-violence research becomes constitutionally relevant, and regulations supported by strong epidemiological evidence (waiting periods, secure storage, extreme-risk orders) would be more likely to survive review. If the right is not empirically gated, harm evidence becomes policy argument rather than constitutional fact, and beneficiaries retain regulatory veto regardless of harm magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_necessity_empirical_gate, empirical, 'Whether the pre-existing individual right is subject to empirical validation of regulatory necessity.').

omega_variable(
    harm_invisibility_in_constitutional_frame,
    'Is the constraint''s treatment of harm from unrestricted access (mass shooting, domestic violence lethality, firearm suicide completion rates) as policy questions rather than constitutional facts a feature or a bug of the individual-right reading?',
    'The reading''s proponents argue harm is policy-relevant but not constitutionally dispositive; harm-based critics argue that a robust constitutional reading would account for harm to other constitutional rights (life, bodily security, equal protection). This is not empirically resolvable but depends on normative framing: should constitutional rights be balanced against demonstrable harms, or are rights presumptively trumps?',
    'If harms should enter constitutional calculus, the reading''s classification would shift toward snare (pure extraction with harm cover). If rights are presumptive trumps, the reading''s extraction is justified by the coordination benefit of constitutional stability. Current Supreme Court doctrine treats the right as a trump, but this remains contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_invisibility_in_constitutional_frame, preference, 'Normative framing of whether demonstrable harms from unrestricted access should weigh in constitutional review, or whether rights are presumptive trumps.').

omega_variable(
    alternative_constitutional_reading_foreclosure,
    'Is the individual-right reading structurally foreclosing, such that a single legal framework cannot coherently hold both this reading and the militia-conditioned reading? Or do they coexist as alternative readings available to different institutional actors?',
    'Supreme Court en banc reconsideration of Heller and McDonald; legislative constitutional amendment; documented shift in judicial interpretation across state and federal appellate courts. At present, the Supreme Court has locked in the individual-right reading as binding, but lower courts and international comparison show that militia-conditioned readings remain live in other jurisdictions.',
    'If foreclosing: the readings are genuine logical alternatives and the choice between them is the central constitutional contest. If coexisting: both readings are available and institutional actors choose which frame to apply in their domain. Current US constitutional law treats individual-right as binding precedent, but comparative constitutional law shows the readings are not logically foreclosed — other jurisdictions hold stable militia-conditioned frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_constitutional_reading_foreclosure, conceptual, 'Structural relationship between the individual-right and militia-conditioned readings — whether they logically foreclose each other or coexist as available alternatives.').

omega_variable(
    beneficiary_identity_fusion,
    'To what extent does the individual-right reading constitute the identity and organizational structure of gun-rights advocates and manufacturers, such that exiting the reading would dissolve the agent itself?',
    'Post-precedent analysis: if a future Court overturned the individual-right reading and adopted militia-conditioned interpretation, would gun-rights organizations and manufacturers maintain their current structure and advocacies, or would they dissolve/reorganize fundamentally? Historical precedent: when Roe v. Wade was overturned, did abortion-advocacy organizations reorganize or dissolve?',
    'If identity-fused: beneficiaries are identity-locked to defending the reading regardless of marginal policy costs, which sustains extractive structures despite high social resistance. If identity-fungible: beneficiaries could exit the reading if costs rose, which would reduce the constraint''s extractive resilience. High identity fusion would explain why the reading persists despite sustained opposition and acknowledged harms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, empirical, 'Degree to which beneficiary organizations are identity-fused to the individual-right reading, making exit structurally impossible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t3, second_amendment_boundary__individual_right_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(seco_tr_t3, observed).
narrative_ontology:measurement(seco_tr_t6, second_amendment_boundary__individual_right_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement_basis(seco_tr_t6, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__individual_right_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__individual_right_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__individual_right_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(seco_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t3, second_amendment_boundary__individual_right_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(seco_be_t3, observed).
narrative_ontology:measurement(seco_be_t6, second_amendment_boundary__individual_right_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement_basis(seco_be_t6, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__individual_right_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__individual_right_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__individual_right_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(seco_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t3, second_amendment_boundary__individual_right_reading, suppression_requirement, 3, 0.37).
narrative_ontology:measurement_basis(seco_su_t3, observed).
narrative_ontology:measurement(seco_su_t6, second_amendment_boundary__individual_right_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement_basis(seco_su_t6, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__individual_right_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__individual_right_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__individual_right_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(seco_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearm_access_regulatory_authority).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, mass_shooting_prevention_policy).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, domestic_violence_lethality_intervention).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel contains three structurally distinct constraint readings: individual-right (this file), militia-conditioned, and insurrectionist. Each reading instantiates a different constraint with different beneficiary/victim sets, different extraction profiles, and different classifications. They share the contested text (the Amendment) but diverge in scope, justification, and operative authority. The individual-right reading is upstream in institutional dominance (current Supreme Court binding doctrine) but influences rather than forecloses the militia-conditioned reading (which remains live in lower courts, international jurisdictions, and academic doctrine). The three readings are linked via affects_constraints to show the kernel family structure and to enable contamination analysis: if the individual-right reading's legitimacy erodes, pressure propagates to alternative readings and to the regulatory-authority constraints that the readings structurally enable or constrain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
