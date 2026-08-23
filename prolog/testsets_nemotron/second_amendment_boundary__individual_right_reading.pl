% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Second Amendment Individual Right Reading (Heller/McDonald Line)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the individual right reading of the
 *   Second Amendment as established in District of Columbia v. Heller (2008)
 *   and extended in McDonald v. Chicago (2010) and New York State Rifle &
 *   Pistol Association v. Bruen (2022). The reading holds that the operative
 *   clause ('the right of the people to keep and bear Arms, shall not be
 *   infringed') establishes a pre-existing individual right to possess
 *   firearms for self-defense, and that the prefatory militia clause ('A well
 *   regulated Militia, being necessary to the security of a free State')
 *   states a purpose but imposes no limitation on the right's scope. The
 *   constraint operates by treating any firearms regulation as presumptively
 *   unconstitutional unless the government demonstrates historical tradition
 *   analogues — a test that blocks modern supply-side regulations (universal
 *   background checks, assault weapons bans, high-capacity magazine
 *   restrictions, waiting periods, safe storage mandates, red flag laws) that
 *   lack 1791/1868 analogues. The claimed type is tangled_rope: the reading
 *   provides genuine coordination (a stable floor against total bans, a
 *   structured framework for judicial review) while simultaneously extracting
 *   asymmetric costs from victim populations who bear the mortality and
 *   morbidity burden of the unrestricted access the reading protects. The
 *   extraction has intensified over the interval as the test evolved from
 *   Heller's 'longstanding prohibitions' framework to Bruen's 'history and
 *   tradition' test, expanding the range of blocked regulations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.48).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading (Heller/McDonald Line)").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'bac518ce-3133-4be3-817d-4f4caabdd505').
narrative_ontology:cs_kernel_codification('bac518ce-3133-4be3-817d-4f4caabdd505', fixed_text).
narrative_ontology:cs_authority_grounding('bac518ce-3133-4be3-817d-4f4caabdd505', lineage).
narrative_ontology:cs_interpretation_layer_present('bac518ce-3133-4be3-817d-4f4caabdd505').
narrative_ontology:cs_reading_relation('bac518ce-3133-4be3-817d-4f4caabdd505', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('bac518ce-3133-4be3-817d-4f4caabdd505', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('bac518ce-3133-4be3-817d-4f4caabdd505', foundational, individual_right_to_arms_preexisted_constitution).
narrative_ontology:cs_axiom_status(individual_right_to_arms_preexisted_constitution, holdable).
narrative_ontology:cs_axiom_grounding('bac518ce-3133-4be3-817d-4f4caabdd505', individual_right_to_arms_preexisted_constitution, deontological).
narrative_ontology:cs_axiom('bac518ce-3133-4be3-817d-4f4caabdd505', foundational, prefatory_clause_does_not_limit_operative_clause).
narrative_ontology:cs_axiom_status(prefatory_clause_does_not_limit_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('bac518ce-3133-4be3-817d-4f4caabdd505', prefatory_clause_does_not_limit_operative_clause, deontological).
narrative_ontology:cs_reference_frame('bac518ce-3133-4be3-817d-4f4caabdd505', founding_era_individual_self_defense_right).
narrative_ontology:cs_drift_state('bac518ce-3133-4be3-817d-4f4caabdd505', post_bruen_history_tradition_test, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bac518ce-3133-4be3-817d-4f4caabdd505', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, pro_gun_advocacy_organizations).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, law_abiding_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims_families).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, communities_affected_by_gun_violence).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, law_enforcement_officers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, supreme_court_originalist_majority).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, law_abiding_gun_owners).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, individual_right_to_arms_preexisted_constitution).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, prefatory_clause_does_not_limit_operative_clause).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, strict_scrutiny_for_core_second_amendment_burdens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and enforces the individual right reading through judicial review. The majority's institutional power derives from life tenure and the Court's final say on constitutional meaning. They benefit from the reading's legitimation of their interpretive method (originalism) and its political alignment with their appointing coalitions. Exit is analytical — they could adopt a different reading but face enormous institutional and reputational costs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, supreme_court_originalist_majority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, supreme_court_originalist_majority, beneficiary).

% Direct financial beneficiaries of constitutionally shielded market access. The individual right reading blocks supply-side regulations (bans, licensing, waiting periods) that would reduce sales volume. They fund litigation and advocacy through NRA-ILA and NSSF. Exit is arbitrage-grade — they can shift product lines, emphasize accessories, or relocate manufacturing if specific regulations pass, but the constitutional floor protects their core market.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry_manufacturers, beneficiary,
    organized, biographical, arbitrage, national).

% Local and regional FFL holders who depend on unrestricted retail channels. The reading protects their business model against jurisdiction-by-jurisdiction regulation. Exit is constrained — they are tied to physical locations and state licensing regimes, but can shift to online sales or accessories if specific products are restricted.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_retailers, beneficiary,
    moderate, biographical, constrained, regional).

% NRA, GOA, FPC, SAF — organizations whose institutional identity, fundraising, and membership mobilization are fused to the individual right reading. They benefit from the reading's legal victories (donor validation, membership growth). Exit is identity-locked: abandoning the reading would dissolve their organizational purpose and donor base. Their power comes from voter mobilization and litigation funding.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, pro_gun_advocacy_organizations, beneficiary,
    organized, biographical, identity_locked, national).

% Individuals who possess firearms for self-defense, recreation, or collecting. They benefit from legal clarity protecting their possessions from confiscation or burdensome regulation. They also pay compliance costs (background checks, training mandates, transfer fees) that the reading permits but does not forbid. Exit is constrained — they can sell firearms or move jurisdictions, but their identity and community ties are often firearms-adjacent.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, law_abiding_gun_owners, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, law_abiding_gun_owners, payer).

% Families and survivors of mass shootings enabled by unrestricted access to high-capacity firearms. They bear the ultimate cost of the reading's market protection: loved ones killed by weapons the reading shields from meaningful supply-side regulation. Exit is trapped — grief and trauma cannot be exited; advocacy is the only channel, which faces structural opposition from the reading's beneficiaries.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims_families, payer,
    powerless, biographical, trapped, national).

% Intimate partners and family members at elevated lethality risk when abusers have unrestricted firearm access. The reading's presumption against regulation blocks universal background checks, red flag laws, and relinquishment protocols that save lives. Exit is trapped — leaving the relationship does not remove the firearm threat; the constraint's protection of abuser access follows them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, national).

% Individuals in suicidal crisis whose access to firearms — protected by the reading's barrier to waiting periods, safe storage mandates, and voluntary relinquishment — converts attempt to completion with ~90% case fatality. They pay with their lives. Exit is trapped — the constraint operates at the moment of crisis; no structural exit exists for the suicidal person.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access, payer,
    powerless, immediate, trapped, national).

% Urban and suburban communities experiencing endemic gun homicide, shootings, and trauma. They bear collective costs: healthcare, lost productivity, psychological burden, disinvestment. They organize politically (Moms Demand, Everytown, community violence intervention) but face structural disadvantages: gerrymandering, Senate malapportionment, industry lobbying. Exit is constrained — they can relocate but lose community, wealth, and networks.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, communities_affected_by_gun_violence, payer,
    moderate, biographical, constrained, regional).

% Police who face elevated threat environments due to unrestricted firearm circulation. The reading blocks regulations (universal background checks, assault weapons restrictions, high-capacity magazine bans) that would reduce officer shootings and line-of-duty deaths. Police unions have historically supported some regulations but are politically captured by the same coalitions that benefit from the reading. Exit is constrained — they cannot leave the threat environment without leaving the profession.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, law_enforcement_officers, payer,
    organized, biographical, constrained, national).

% Academics who study the Second Amendment's text, history, and doctrine. They do not collect rents or bear direct costs from the reading's operation. Their work shapes the intellectual environment in which courts and advocates operate. Exit is analytical — they can change positions without institutional penalty, though reputational stakes exist.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable constitutional floor for firearms possession and commerce, resolving the pre-Heller ambiguity that allowed jurisdictions to ban handguns entirely. Coordinates expectations of gun owners, manufacturers, retailers, and regulators around a baseline of protected possession (handguns in the home for self-defense) and a structured framework for evaluating regulations (text, history, tradition).
% TRANSFER_FUNCTION: Moves regulatory authority from legislatures (state and local) to courts, shifting the power to define permissible firearms policy from democratic majorities to judicial interpretation of historical tradition. Transfers risk of firearm injury and death from the regulated market (where supply-side restrictions would reduce circulation) to the victim populations who bear the consequences of unrestricted access. Transfers litigation costs and compliance burdens to governments defending regulations and individuals navigating the resulting patchwork.
% ABSENT_VOICES: Future victims of shootings not yet occurred — the reading's operation creates victims who do not yet exist and therefore cannot be present in any adjudication. Non-voting populations (minors, non-citizens, incarcerated persons) disproportionately affected by community gun violence but excluded from the political process that the reading empowers courts to override. International comparative voices (other democracies with lower firearm homicide rates under different regulatory regimes) are structurally excluded from U.S. constitutional discourse.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, the pre-Heller regulatory landscape would return: jurisdictions could ban handguns, impose licensing, require waiting periods, restrict high-capacity magazines, and mandate safe storage without strict scrutiny review. The firearms market would face supply-side regulation; mass shooting lethality would likely decrease over time as weapon and magazine restrictions took effect; domestic violence lethality would drop with relinquishment enforcement; suicide completion rates would fall with waiting periods and storage laws. The political economy of gun advocacy would collapse without its constitutional anchor.
% FOUNDING_PROBLEM: The pre-Heller era produced total handgun bans (D.C., Chicago) that the individual right reading's proponents viewed as destroying the core of the Second Amendment — the right of law-abiding citizens to possess functional firearms for self-defense in the home. The founding problem was the absence of any judicially enforceable floor against complete prohibition.
% FOUNDING_PROBLEM_CORROBORATION: The individual right reading's proponents (originalist scholars, gun rights organizations) attest the founding problem remains live — they argue any regulatory erosion risks returning to total bans. Critics (living constitutionalists, public health researchers, dissenting justices in Heller/McDonald/Bruen) attest the founding problem is substantially solved: no jurisdiction seeks total handgun bans today; the reading now functions to block regulations (universal background checks, assault weapons bans, red flag laws) that have no relationship to the founding problem. Independent historical scholarship (e.g., Cornell, Waldman, Rakove) corroborates that the founding-era understanding was militia-conditioned, not individual-right.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.62) reflects the reading's blocking of supply-side regulations that public health evidence shows would reduce firearm homicide, suicide, and domestic violence lethality. The extraction is not total (hence not snare) because the reading permits some regulations (prohibitions on felon possession, sensitive place restrictions, commercial sale conditions) and provides genuine coordination value (predictable floor for lawful possession). Suppression (0.48) is moderate: the constraint operates through judicial invalidation rather than direct coercion, but the Bruen test's rigidity suppresses legislative experimentation. Theater ratio (0.28) captures the growing gap between the reading's stated originalist methodology and its selective historical analysis — the 'history and tradition' test functions increasingly as a proxy for policy preference. Accessibility collapse (0.38) is moderate: alternatives (militia-conditioned reading, intermediate scrutiny) persist in dissent, scholarship, and lower court resistance, but the Supreme Court's current composition makes doctrinal reversal unlikely. Resistance (0.71) is high: state and local governments, public health advocates, and victim organizations actively litigate, legislate, and organize against the reading's expansion.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent per-seat classifications from this structural data. From the agenda_setter (originalist majority) and organized beneficiary (industry, advocacy) seats, the constraint appears as genuine coordination (rope-like): a stable constitutional rule resolving ambiguity, protecting a pre-existing right, enabling a lawful market. From the payer seats (victim populations, law enforcement), the same structure computes as extractive (snare-like): a rigid test that blocks life-saving regulations, shields a lethal market, and transfers mortality risk to the powerless. From the dual-positioned law-abiding gun owner seat (beneficiary/payer), the constraint computes as genuinely tangled — real protection for their possessions, real costs from the unregulated environment. The claimed tangled_rope type reflects this structural asymmetry; the engine's per-seat computation will reveal it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court originalist majority (agenda_setter) sits at the beneficiary end of directionality (d ≈ 0.15): they gain institutional legitimacy for their interpretive method and political alignment with appointing coalitions. Firearms industry manufacturers (beneficiary, organized, arbitrage exit) sit near d ≈ 0.1 — they capture the commercial value of constitutionally shielded market access with high exit flexibility. Pro-gun advocacy organizations (beneficiary, organized, identity_locked exit) sit at d ≈ 0.05 — their institutional existence is fused to the reading; they cannot exit without dissolution. Law-abiding gun owners (beneficiary/payer, moderate, constrained exit) sit near symmetric d ≈ 0.5 — they gain possession security but pay compliance costs and bear some risk. All victim populations (payer, powerless/moderate, trapped/constrained exit) sit at the target end d ≈ 0.85-0.95 — they bear mortality, morbidity, and community costs with minimal structural exit. Law enforcement (payer, organized, constrained exit) sits at d ≈ 0.7 — they face elevated threat environments but have institutional voice (though politically captured). Constitutional scholars (observer, analytical) sit at d ≈ 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (total handgun bans) is substantially solved — no jurisdiction seeks such bans today. The constraint persists and expands (Bruen) to block regulations unrelated to the founding problem (universal background checks, red flag laws, assault weapons restrictions). This is mandatrophy: the arrangement's mandate (preventing total prohibition) has been achieved, but the constraint continues to operate and grow, extracting increasing costs from victim populations. The originalist majority's refusal to acknowledge this drift — insisting the founding problem remains 'live' because any regulation could theoretically lead to prohibition — is the cover story that sustains the extraction. The reading_relations and axioms in cs_structure formalize how this reading forecloses the militia-conditioned alternative that would permit the regulations the victims need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the Second Amendment kernel admit the individual right reading as a structurally stable interpretation, or is the reading a constructed imposition on an irreducibly militia-conditioned text?',
    'Comparative constitutional analysis: if other constitutional rights with prefatory clauses (none exist in U.S. Constitution) or analogous founding-era state constitutions consistently treat prefatory clauses as limiting, the individual right reading is a constructed departure. Historical linguistic analysis of ''bear arms'' corpus data.',
    'If the reading is a constructed imposition, its claimed coordination function (resolving ambiguity) is false — it creates the ambiguity it claims to resolve. The constraint would reclassify toward snare (pure extraction) from the analytical seat. If the reading is structurally stable, the coordination function is genuine and tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the individual right reading is a genuine resolution of textual ambiguity or a constructed imposition on a militia-conditioned kernel.').

omega_variable(
    victim_set_boundary,
    'Where does the victim set boundary lie? Are only direct shooting victims payers, or do communities bearing collective trauma, healthcare costs, and disinvestment also count?',
    'Public health economics: measure the full social cost of firearm injury (medical, lost productivity, quality-of-life, community-level effects) attributable to regulations blocked by the reading. Epidemiological studies of community trauma exposure.',
    'A broader victim set increases the constraint''s measured extraction and strengthens the snare classification from payer seats. A narrower set (only direct shooting victims) reduces extraction but still supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Scope of the victim set for extraction accounting.').

omega_variable(
    history_tradition_test_manipulability,
    'Is the Bruen ''history and tradition'' test a genuine constraint on judicial discretion, or a manipulable proxy for policy preference that selectively credits analogues?',
    'Empirical analysis of post-Bruen lower court decisions: coding whether judges appointed by different parties reach systematically different outcomes on identical regulatory challenges using the same historical record. Inter-coder reliability studies of historical analogue identification.',
    'If manipulable, the theater_ratio is underestimated — the constraint''s performative originalism masks policy-driven extraction. The coordination function degrades further toward snare. If genuine, the test provides real coordination (predictable outcomes) and the theater_ratio is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(history_tradition_test_manipulability, empirical, 'Whether the history and tradition test constrains judicial discretion or channels it.').

omega_variable(
    coordination_extraction_separability,
    'Can the reading''s coordination function (floor against total bans, structured review framework) be separated from its extraction function (blocking life-saving supply-side regulations)?',
    'Counterfactual doctrinal design: could a court articulate a standard that preserves the anti-prohibition floor (Heller''s core) while applying intermediate scrutiny to supply-side regulations? Historical analysis of pre-Heller lower court standards that achieved this.',
    'If separable, the tangled_rope classification is precise — genuine coordination + asymmetric extraction are structurally distinct components. If inseparable, the coordination function may be a necessary cost of the extraction (or vice versa), complicating the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or inextricably fused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sam_indiv_right_tr_t2008, second_amendment_boundary__individual_right_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(sam_indiv_right_tr_t2010, second_amendment_boundary__individual_right_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(sam_indiv_right_tr_t2016, second_amendment_boundary__individual_right_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(sam_indiv_right_tr_t2020, second_amendment_boundary__individual_right_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(sam_indiv_right_tr_t2022, second_amendment_boundary__individual_right_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(sam_indiv_right_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.27).
narrative_ontology:measurement(sam_indiv_right_tr_t2026, second_amendment_boundary__individual_right_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(sam_indiv_right_be_t2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 2008, 0.18).
narrative_ontology:measurement(sam_indiv_right_be_t2010, second_amendment_boundary__individual_right_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(sam_indiv_right_be_t2016, second_amendment_boundary__individual_right_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(sam_indiv_right_be_t2020, second_amendment_boundary__individual_right_reading, base_extractiveness, 2020, 0.49).
narrative_ontology:measurement(sam_indiv_right_be_t2022, second_amendment_boundary__individual_right_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement(sam_indiv_right_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(sam_indiv_right_be_t2026, second_amendment_boundary__individual_right_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sam_indiv_right_su_t2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 2008, 0.25).
narrative_ontology:measurement(sam_indiv_right_su_t2010, second_amendment_boundary__individual_right_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(sam_indiv_right_su_t2016, second_amendment_boundary__individual_right_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(sam_indiv_right_su_t2020, second_amendment_boundary__individual_right_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(sam_indiv_right_su_t2022, second_amendment_boundary__individual_right_reading, suppression_requirement, 2022, 0.45).
narrative_ontology:measurement(sam_indiv_right_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.47).
narrative_ontology:measurement(sam_indiv_right_su_t2026, second_amendment_boundary__individual_right_reading, suppression_requirement, 2026, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_regulation_preemption_doctrine).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, state_preemption_of_local_gun_laws).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, protection_of_lawful_commerce_in_arms_act).

% DUAL FORMULATION NOTE:
% This constraint is one member of the second_amendment_boundary kernel family. The three readings (individual_right, militia_conditioned, insurrectionist) instantiate structurally distinct constraints from the same textual kernel. This reading's ε (0.62) is substantially higher than the militia_conditioned_reading's ε would be (near 0.05 — it permits comprehensive regulation), and differs from the insurrectionist_reading's ε (estimated 0.45 — it protects resistance-capable arms but may permit regulation of non-resistance arms). The ε-invariance principle requires separate stories because the observable (which regulations are blocked) changes ε. All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, institutional, 0.15).
constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, organized, 0.1).
constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, moderate, 0.5).
constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, powerless, 0.9).
constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
