% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Divine Legitimacy Monopoly
 *   domain: religious/political_economy
 *
 * SUMMARY:
 *   The Atenist monotheistic reading claims that divine legitimacy flows
 *   exclusively through pharaonic revelation of Aten, and that all other
 *   deities are false. This constraint instantiates that reading as a
 *   political-theological system: the pharaoh seizes interpretive monopoly
 *   over the divine realm, dismantles independent temple priesthoods,
 *   suppresses folk deities and household practice, and redistributes
 *   religious property and authority to the pharaonic administration. The
 *   constraint combines a genuine coordination function (unified spiritual
 *   authority replacing theological competition) with substantial extraction
 *   (property seizure, revenue consolidation, suppression of alternative
 *   authority structures). This is ONE reading of the contested
 *   divine-legitimacy kernel — the other readings (Amun polytheistic, folk
 *   syncretistic) would instantiate different constraints with different
 *   beneficiary/victim structures and types. The authored metrics describe
 *   the Atenist reading's operation as substantially extractive and enforced;
 *   the claim (tangled_rope) reflects this structural asymmetry. The sibling
 *   readings are different constraints, not different observations of the
 *   same constraint.
 *
 * KEY AGENTS:
 *   - pharaonic_authority: Institutional agenda-setter, collects consolidated religious and economic authority, maintains the monopoly through active suppression and theological enforcement.
 *   - temple_priesthoods: Powerful institutional payer, loses autonomy and property, cannot exit without forfeiting centuries-long role and institutional existence.
 *   - folk_practitioners: Powerless identity-locked payers, religious identity fused with family practice, exit means severing lineage continuity.
 *   - elite_court_theologians: Organized beneficiaries, rise through proximity to pharaonic authority, intellectually justify suppression, career entirely dependent on constraint persistence.
 *   - common_population: Powerless beneficiaries with payer secondary role, told they gain spiritual clarity but lose localized alternative worship, constrained to official Aten worship.
 *   - competing_royal_factions: Excluded powerful agents whose power-building paths (leveraging regional religious authority) are foreclosed by the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.87).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.91).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Divine Legitimacy Monopoly").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '9716356e-a39e-4af4-8311-bb25e7f3d27c').
narrative_ontology:cs_kernel_codification('9716356e-a39e-4af4-8311-bb25e7f3d27c', fixed_text).
narrative_ontology:cs_authority_grounding('9716356e-a39e-4af4-8311-bb25e7f3d27c', extraction).
narrative_ontology:cs_interpretation_layer_present('9716356e-a39e-4af4-8311-bb25e7f3d27c').
narrative_ontology:cs_reading_relation('9716356e-a39e-4af4-8311-bb25e7f3d27c', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('9716356e-a39e-4af4-8311-bb25e7f3d27c', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('9716356e-a39e-4af4-8311-bb25e7f3d27c', foundational, aten_exclusive_divinity).
narrative_ontology:cs_axiom_status(aten_exclusive_divinity, holdable).
narrative_ontology:cs_axiom_grounding('9716356e-a39e-4af4-8311-bb25e7f3d27c', aten_exclusive_divinity, theological).
narrative_ontology:cs_axiom('9716356e-a39e-4af4-8311-bb25e7f3d27c', foundational, pharaonic_sole_legitimate_interpreter).
narrative_ontology:cs_axiom_status(pharaonic_sole_legitimate_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('9716356e-a39e-4af4-8311-bb25e7f3d27c', pharaonic_sole_legitimate_interpreter, deontological).
narrative_ontology:cs_reference_frame('9716356e-a39e-4af4-8311-bb25e7f3d27c', aten_monotheistic_orthodoxy_regime).
narrative_ontology:cs_drift_state('9716356e-a39e-4af4-8311-bb25e7f3d27c', post_pharaonic_death_or_successor_transition, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('9716356e-a39e-4af4-8311-bb25e7f3d27c', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_priesthoods).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, minor_deity_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, elite_court_theologians).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, common_population).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, economic_beneficiaries).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, common_population).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_divine_intermediacy).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, monotheistic_theological_truth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares itself the sole legitimate interpreter of divine will through personal revelation of Aten. Issues decrees banning alternative worship, seizes temple property and revenues, redirects priesthood functions through the royal household, and justifies suppression as spiritual correction. Collects consolidated religious authority and economic rents from dissolved temple economies. The pharaoh's power depends on maintaining the monopoly claim.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority, agenda_setter,
    institutional, generational, analytical, national).

% Lose institutional autonomy, property holdings, tax-collection authority, and interpretive legitimacy. Their centuries-long function as mediators between divine and human is declared false and criminalized. They can submit to incorporation under pharaonic oversight (forfeiting autonomy) or face confiscation and exile. Their theological position — that multi-deity cosmology is the legitimate framework — is treated as rebellion.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_priesthoods, payer,
    powerful, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_priesthoods, excluded).

% Household and village rituals invoking local deities and protective spirits are declared heresy. Their religious identity fused with family and community practice now marks them as enemies of state orthodoxy. They can renounce their tradition (severing family religious continuity) or hide practice and face prosecution. Exit means severing identity from lineage itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Regional and local cult centers serving specific deities — healers, fertility goddesses, protective spirits — are shut down, priests displaced, sacred sites redirected or destroyed. Their religious function (addressing localized needs not covered by Aten monotheism) is no longer legally available. These are non-agent entities functioning through their priesthoods; suppression of their cults is suppression of alternative interpretive authority.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, minor_deity_cults, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(divine_legitimacy_substrate__atenist_monotheistic_reading, minor_deity_cults).

% Rise to power as the pharaoh's approved theological interpreter. They develop the Aten monotheistic doctrine, justify the suppression through theological argument, and administer the ideological transition. They benefit from proximity to pharaonic authority and intellectual prestige, but their entire career depends on the constraint's persistence — they cannot exit without losing status.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, elite_court_theologians, beneficiary,
    organized, biographical, constrained, national).

% Are told they receive spiritual clarity and unified divine guidance through the pharaoh's revelation. They retain some localized practice in private, but public religious life is reorganized around Aten worship directed from the palace. Their spiritual needs formerly served by folk cults and minor deities are officially told to be met by Aten monotheism; non-compliance is suppressed.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, common_population, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, common_population, payer).

% Royal administrators, military connected to the court, and favored merchants gain access to confiscated temple lands and revenues. The consolidation of religious power into pharaonic hands also concentrates wealth redistribution through court patronage. They benefit from the constraint but are not its primary administrator.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, economic_beneficiaries, beneficiary,
    powerful, generational, arbitrage, national).

% Powerful families and regional governors whose authority rested partly on alliance with temple priesthoods or regional deity cults lose clients and patronage networks when those institutions are dismantled. They are prevented from leveraging alternative religious authority to build power bases; the constraint forecloses that path.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, competing_royal_factions, excluded,
    powerful, biographical, trapped, national).

% Analyze whether the constraint operated as stated — whether Aten monotheism was structurally enforced, how complete the suppression was, what happened to alternative practice, and whether the theological claims masked economic consolidation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies religious cosmology under a single authoritative source: removes theological competition, simplifies ritual practice under state administration, centralizes spiritual legitimacy through a single interpretive body (the pharaoh) rather than distributed temple priesthoods and folk practice.
% TRANSFER_FUNCTION: Moves religious authority, temple property, tax revenues, and spiritual legitimacy from independent priesthoods and folk practitioners to pharaonic administration; also redistributes confiscated resources to court-connected elites and military.
% ABSENT_VOICES: Displaced priesthoods and folk practitioners are excluded — they would argue the constraint is theological oppression disguised as divine truth. Competing royal factions would argue alternative religious frameworks should remain available. Those voices are structurally silenced by enforcement.
% DISAPPEARANCE_RATIONALE: If the monopoly vanished, the priesthoods would immediately reinstitute their temples, folk practitioners would resume household rituals openly, minor deity cults would reconstruct, regional power networks would leverage renewed religious autonomy. The entire architecture of consolidated pharaonic-religious power would decompose within years.
% FOUNDING_PROBLEM: Theological plurality across competing priesthoods and local cults created interpretive authority chaos: which deity is supreme, which priests are legitimate, which rituals are effective? Multiple centers of religious authority fragmented spiritual legitimacy and enabled competing claims.
% FOUNDING_PROBLEM_CORROBORATION: The pharaoh's theologians attest the problem was severe and monotheism the solution. Displaced priesthoods and folk practitioners attest the diversity was functional and the 'problem' was manufactured to justify consolidation. External comparative analysis of other ancient polytheistic systems shows theological plurality coexisting stably without collapse, suggesting the founding problem was overstated or constructed.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.87 terminal, climbing from 0.68) because the constraint transfers religious authority, temple property, and revenue streams from independent institutions to pharaonic administration — the transfer is decoupled from any genuine coordination cost differential and represents a power consolidation via theological monopoly. Suppression is highest among all metrics (0.91) because the constraint's persistence depends entirely on active enforcement: priesthoods are exiled or incorporated under duress, folk practitioners face prosecution, alternative deity cults are shut down and their assets redirected. Theater rises moderately over the interval (0.45 to 0.62): the coordination function (unified spiritual authority) is real in the early phase, but by the endpoint the constraint is mostly machinery for maintaining the monopoly and redistributing resources — coordination fades and enforcement theater rises. Accessibility_collapse is asymmetric across levels: organizational actors (priesthoods, cults) face nearly total foreclosure (0.95 at endpoint), individual practitioners face partial collapse (0.81, with hidden practice remaining viable), and structural alternatives are suppressed but not entirely eliminated (0.79). Resistance peaks at the organizational level (0.89 early) as priesthoods actively oppose, but weakens (0.72 by endpoint) as they are either crushed or incorporated; individual and class resistance remain stable (~0.68–0.71) as underground practice and passive non-compliance persist.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic agenda-setter seat experiences the constraint as implementing theological truth and consolidating rightful authority; directional computation from that seat should yield low d (beneficiary, controllers, consolidating power). The priesthood seat experiences it as suppression of legitimate authority and theft of property; d should be near 1.0 (victims, trapped, exiled or incorporated). Folk practitioners and elite court theologians occupy intermediate positions: theologians are beneficiaries by proximity but trapped by dependence (d moderately low); folk practitioners are powerless victims whose identity-locked exit gives them nowhere to go (d near 1.0). The engine computes these divergences from power + exit + beneficiary/victim declarations. The constraint's type should therefore compute differently per seat: from pharaonic perspective, perhaps a rope-like coordination claim; from priesthood and folk perspective, clearly a snare; from court theologian perspective, tangled_rope (coordination benefit + extraction benefit + dependent status).
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaonic authority: beneficiary (collects rents and consolidation), institutional power, analytical exit (can modify the system at will), so d gravitates toward 0.0 (full subsidy/control). Temple priesthoods: victims (property seized, authority suppressed), powerful initially but trapped (exit means institutional death), so d near 1.0 (full extraction target). Folk practitioners: victims (religious practice criminalized), powerless, identity_locked (exit severs family continuity), so d near 1.0 (full extraction target, compounded by identity fusion). Elite court theologians: beneficiaries (rise to prominence), organized power (court access), but constrained exit (career entirely dependent on constraint), so d moderately low (~0.2–0.35, intermediate between beneficiary and trapped-beneficiary). Common population: structured as secondary beneficiaries (told they gain spiritual clarity) but face payer role costs (loss of alternative worship access); powerless and constrained exit, so d moderately high (~0.55–0.65, intermediate beneficiary with extraction costs). Competing royal factions: excluded (not counted in beneficiary/victim but systematically foreclosed), powerful but trapped, so d would be high if they were seated (~0.75+). No overrides required — the derivation chain handles the asymmetry through power + exit + role declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (theological chaos from competing priesthoods and cults) is attest as contested: the pharaoh's theologians claim it was severe; displaced priesthoods and external analysis claim it was overstated or functional. The disappearance_verdict is world_rearranges (if the monopoly vanished, priesthoods would immediately reinstitute, folk practice would resume, regional power networks would leverage religious autonomy). The mismatch (founding_problem_status=contested with disappearance_verdict=world_rearranges) flags a zombie/capture read: the founding problem is not live; removing the constraint would not solve it but would decompose the current power structure. This suggests the constraint persists as extracted authority consolidation, not as coordination to a genuine necessity. A mandatrophy_resolved verdict would be appropriate: the constraint started coordinating (plural religious authorities genuinely did create legitimacy fragmentation), but the founding problem died or was solved, and the constraint persisted as pure extraction. Theater_ratio plateau at 0.62 suggests moderate performative maintenance by the endpoint: enforcement machinery is partly performance (the Aten theology has become ritual, alternative suppression is partly ceremonial), but extraction machinery remains functional (property redistribution, authority consolidation, priesthood incorporation continue actively).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Is the Atenist monotheistic reading of divine legitimacy structurally enforced religious orthodoxy, or does it genuinely represent a theological truth about the divine?',
    'This is the primary kernel ambiguity: the reading itself claims divine exclusivity; sibling readings claim their own theological truth. The committer frame treats all readings as a priori contestable and locates the reading inside the family of alternatives. Structural data (beneficiaries, victims, enforcement) describe the CONSTRAINT this reading instantiates; they do not adjudicate its theological claim.',
    'If treated as theological truth: the constraint is natural law from this reading''s frame. If treated as constructed orthodoxy: the constraint is tangled_rope (coordination function — unified spiritual authority — plus extraction — property seizure, suppression). The authored claim (tangled_rope) reflects structural analysis, not theological judgment. This omega documents the irreducible reading-frame underspecification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'The kernel-reading underspecification: this constraint is one reading of a contested divine-legitimacy kernel, not an objective natural law.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.91) primarily structural (law enforcement, property seizure, exile) or internalized (subjects accepting the theological framing as true)?',
    'Post-constraint trajectory analysis: what happens to compliance and belief after suppression infrastructure is removed or weakened? If belief persists, suppression is partly internalized; if it immediately erodes, suppression is primarily structural. Historical records of folk practice continuity during enforcement vs. resumption during later pluralistic periods would show the ratio.',
    'If internalized: the constraint''s effective suppression is higher than the structural measure (subjects carry it). If structural: the constraint requires continuous enforcement machinery. This affects whether the constraint self-stabilizes or requires ongoing investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural (external force) or internalized (adopted belief).').

omega_variable(
    beneficiary_stability_pharaonic_authority,
    'Does pharaonic authority genuinely benefit from the Aten monopoly, or does the constraint persist because dismantling it is politically harder than maintaining it?',
    'Cost-benefit analysis of constraint maintenance vs. dissolution: enforcement costs, religious legitimacy gains, economic rents from temple property, political stability under unified authority vs. costs of suppression and theological justification. Do pharaonic successor regimes voluntarily maintain the constraint, or do they immediately revert to pluralism when the original pharaoh''s direct authority fades?',
    'If genuinely beneficial: the constraint is a snare with a durable beneficiary (the pharaonic office itself). If inertially maintained: it is a piton, persisting despite costs because the original architect created dependencies that would be expensive to unwind. This affects type classification and persistence prediction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_stability_pharaonic_authority, empirical, 'Whether the pharaonic office benefits enough to maintain the constraint independently, or benefits are exhausted and inertia remains.').

omega_variable(
    folk_practice_survival_rate,
    'How completely was folk practice actually suppressed? Did it go underground, persist under theological cover, or genuinely disappear?',
    'Archaeological and textual evidence of household deity invocations, protective amulets, and local shrine maintenance during the enforcement period. If these artifacts/references persist, folk practice survived suppression; if they vanish and reappear only after the constraint lifts, suppression was nearly complete.',
    'Complete suppression supports the high accessibility_collapse (0.78–0.95) and high theater_ratio (0.62). Partial survival suggests lower effective suppression and lower theater (people maintained the alternatives despite official prohibition). This affects whether the constraint truly collapses alternative access or merely drove it underground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_practice_survival_rate, empirical, 'The completeness of folk-practice suppression — total elimination vs. underground persistence.').

omega_variable(
    theological_manufacturing_vs_revelation,
    'Was Aten monotheism a genuine theological innovation by the pharaoh, or was it manufactured by court theologians to consolidate power?',
    'Comparative analysis of Aten theology with prior theological currents, analysis of which theologians authored the doctrine and their institutional incentives, analysis of whether the theology makes internal sense or appears post-hoc justification for confiscation.',
    'Genuine innovation suggests the constraint had a real theological integrity component; manufacturing suggests it is pure extraction wrapped in theological language. This affects how cleanly the constraint decompose into coordination and extraction sub-functions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_manufacturing_vs_revelation, conceptual, 'Whether Aten monotheism was a genuine theological development or post-hoc justification for consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement_basis(divi_tr_t3, observed).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement_basis(divi_tr_t6, observed).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 10, 0.57).
narrative_ontology:measurement_basis(divi_tr_t10, observed).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.61).
narrative_ontology:measurement_basis(divi_tr_t15, observed).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement_basis(divi_tr_t20, observed).
narrative_ontology:measurement(divi_tr_t25, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(divi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.74).
narrative_ontology:measurement_basis(divi_be_t3, observed).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.79).
narrative_ontology:measurement_basis(divi_be_t6, observed).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 10, 0.84).
narrative_ontology:measurement_basis(divi_be_t10, observed).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.86).
narrative_ontology:measurement_basis(divi_be_t15, observed).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(divi_be_t20, observed).
narrative_ontology:measurement(divi_be_t25, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 25, 0.87).
narrative_ontology:measurement_basis(divi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.84).
narrative_ontology:measurement_basis(divi_su_t3, observed).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.87).
narrative_ontology:measurement_basis(divi_su_t6, observed).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement_basis(divi_su_t10, observed).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement_basis(divi_su_t15, observed).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 20, 0.91).
narrative_ontology:measurement_basis(divi_su_t20, observed).
narrative_ontology:measurement(divi_su_t25, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 25, 0.91).
narrative_ontology:measurement_basis(divi_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(divi_grid_01, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(divi_grid_02, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(class), 25, 0.84).
narrative_ontology:measurement(divi_grid_03, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(divi_grid_04, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(individual), 25, 0.81).
narrative_ontology:measurement(divi_grid_05, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(organizational), 0, 0.85).
narrative_ontology:measurement(divi_grid_06, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(organizational), 25, 0.95).
narrative_ontology:measurement(divi_grid_07, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(divi_grid_08, divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse(structural), 25, 0.79).
narrative_ontology:measurement(divi_grid_09, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement(divi_grid_10, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(class), 25, 0.71).
narrative_ontology:measurement(divi_grid_11, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(divi_grid_12, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(individual), 25, 0.68).
narrative_ontology:measurement(divi_grid_13, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(organizational), 0, 0.89).
narrative_ontology:measurement(divi_grid_14, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(organizational), 25, 0.72).
narrative_ontology:measurement(divi_grid_15, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(divi_grid_16, divine_legitimacy_substrate__atenist_monotheistic_reading, resistance(structural), 25, 0.74).
narrative_ontology:measurement(divi_grid_17, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(divi_grid_18, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(class), 25, 0.82).
narrative_ontology:measurement(divi_grid_19, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(individual), 0, 0.55).
narrative_ontology:measurement(divi_grid_20, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(individual), 25, 0.78).
narrative_ontology:measurement(divi_grid_21, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(organizational), 0, 0.89).
narrative_ontology:measurement(divi_grid_22, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(organizational), 25, 0.98).
narrative_ontology:measurement(divi_grid_23, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(structural), 0, 0.71).
narrative_ontology:measurement(divi_grid_24, divine_legitimacy_substrate__atenist_monotheistic_reading, stakes_inflation(structural), 25, 0.87).
narrative_ontology:measurement(divi_grid_25, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(class), 0, 0.78).
narrative_ontology:measurement(divi_grid_26, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(class), 25, 0.9).
narrative_ontology:measurement(divi_grid_27, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement(divi_grid_28, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(individual), 25, 0.88).
narrative_ontology:measurement(divi_grid_29, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(organizational), 0, 0.91).
narrative_ontology:measurement(divi_grid_30, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(organizational), 25, 0.97).
narrative_ontology:measurement(divi_grid_31, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(structural), 0, 0.81).
narrative_ontology:measurement(divi_grid_32, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression(structural), 25, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% Part of the divine_legitimacy_substrate kernel family. The Atenist reading (this constraint) forecloses the polytheistic reading within its framework (if Aten alone is true, multi-deity cosmology is false) but coexists historically with it because different institutional actors held the readings. The folk syncretistic reading represents a third structural possibility (decentralized, non-monopolized religious authority) that the Atenist reading also forecloses doctrinally but which persisted as underground practice despite suppression. All three readings are constraints; family members are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
