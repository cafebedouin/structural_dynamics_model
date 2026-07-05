% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text as Popular-Sovereignty Reading (Constituent Power Retained by the Demos)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   The popular-sovereignty reading holds that constitutional authority
 *   ultimately derives from the constituent power of the demos, and that
 *   neither judicial review nor legislative supremacy exhausts the sources of
 *   legitimate constitutional change — the people retain a residual,
 *   extra-institutional capacity to reconstitute the order through amendment
 *   campaigns, constitutional conventions, or, in extremis, revolutionary
 *   rupture. This reading genuinely solves a real problem (explaining the
 *   legitimacy of founding moments and preventing permanent institutional
 *   capture of constitutional meaning) but its invocation in settled
 *   democracies increasingly serves as a tool that organized, well-resourced
 *   mobilization campaigns wield against institutions (courts, administrative
 *   bodies, minority protections) that cannot exit the framework and cannot
 *   match popular movements' mobilization capacity.
 *
 * KEY AGENTS:
 *   - mobilized_citizen_movements: Primary beneficiary (organized/mobile) — gains standing and leverage through the reading's legitimation of extra-institutional action
 *   - amendment_campaign_organizers: Agenda-setter (organized/mobile) — administers the practical channel through which constituent power becomes actionable
 *   - judicial_institutions: Primary payer (institutional/trapped) — interpretive finality is denied by the reading's own logic
 *   - minority_rights_claimants: Secondary payer (powerless/trapped) — lacks comparable mobilization capacity and is exposed to majoritarian constitutional revision
 *   - comparative_constitutional_scholars: Analytical observer (analytical/global) — compares operation of the reading across polities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.42).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.38).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text as Popular-Sovereignty Reading (Constituent Power Retained by the Demos)").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '475390ff-28f4-4718-a7e0-f782f20c0709').
narrative_ontology:cs_kernel_codification('475390ff-28f4-4718-a7e0-f782f20c0709', distributed).
narrative_ontology:cs_authority_grounding('475390ff-28f4-4718-a7e0-f782f20c0709', distributed).
narrative_ontology:cs_reading_relation('475390ff-28f4-4718-a7e0-f782f20c0709', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('475390ff-28f4-4718-a7e0-f782f20c0709', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('475390ff-28f4-4718-a7e0-f782f20c0709', foundational, constituent_power_exceeds_constituted_institutions).
narrative_ontology:cs_axiom_status(constituent_power_exceeds_constituted_institutions, holdable).
narrative_ontology:cs_axiom_grounding('475390ff-28f4-4718-a7e0-f782f20c0709', constituent_power_exceeds_constituted_institutions, deontological).
narrative_ontology:cs_axiom('475390ff-28f4-4718-a7e0-f782f20c0709', foundational, no_institutional_organ_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_institutional_organ_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('475390ff-28f4-4718-a7e0-f782f20c0709', no_institutional_organ_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('475390ff-28f4-4718-a7e0-f782f20c0709', founding_moment_constituent_assembly).
narrative_ontology:cs_drift_state('475390ff-28f4-4718-a7e0-f782f20c0709', contemporary_mobilization_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('475390ff-28f4-4718-a7e0-f782f20c0709', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, constitutional_convention_delegates).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, amendment_campaign_organizers).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_institutions).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, professional_legal_class).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, administrative_technocracy).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_constitutionalism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize referenda, constitutional conventions, and mass mobilization campaigns to force reinterpretation or amendment of constitutional text outside ordinary legislative or judicial channels. When successful, they establish that no institutional reading of the text is final. Their leverage depends entirely on their capacity to mobilize; between mobilizations they have no standing seat in the ordinary machinery.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements, agenda_setter).

% Set the practical agenda for invoking Article V-style or convention mechanisms, translating diffuse popular sentiment into formal amendment proposals. They administer the sole recognized channel through which the popular-sovereignty reading becomes actionable, and thereby gain outsized influence over which popular claims get institutionalized.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, amendment_campaign_organizers, agenda_setter,
    organized, generational, mobile, national).

% Participate directly in the extra-institutional constitution-making moments this reading privileges. They gain standing and authority precisely because the reading holds that conventions can override settled judicial or legislative interpretation, but their power is episodic and dissolves once the convention ends.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_convention_delegates, beneficiary,
    moderate, biographical, constrained, national).

% Their interpretive rulings are treated as provisional and revisable by popular constitutional moments rather than final. They cannot exit the constraint — courts cannot simply refuse to be subordinated to constituent power without abandoning their own claim to derive authority from the same constitutional text. Every landmark ruling exists under the shadow of possible popular override.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_institutions, payer,
    institutional, generational, trapped, national).

% Lawyers, scholars, and jurists whose professional authority rests on stable, technically mediated constitutional interpretation bear reputational and functional costs when popular mobilization bypasses doctrinal analysis. They can lobby or theorize against the reading but cannot exit the constitutional order in which it operates.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, professional_legal_class, payer,
    powerful, biographical, constrained, national).

% Depend on stable judicial protection of enumerated rights against majoritarian pressure. Under the popular-sovereignty reading, rights guarantees are themselves subject to eventual reversal by sufficiently mobilized popular constitutional moments, leaving them exposed whenever mobilization runs against their interests. They have no independent mobilization capacity comparable to majority movements.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Regulatory and administrative bodies that rely on settled constitutional doctrine to plan and execute policy over multi-year horizons experience the reading as a source of legitimacy uncertainty, since any settled arrangement could in principle be unwound by a subsequent popular constitutional moment.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, administrative_technocracy, payer,
    institutional, biographical, constrained, national).

% The ordinary legislature is not the meta-authority under this reading — it is itself subordinate to constituent power. It would object that this subordination makes ordinary legislative supremacy or notwithstanding mechanisms secondary to street or ballot mobilization, but the reading does not seat the legislature as final arbiter, so its objection has no institutional channel within this reading's own logic.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, excluded,
    institutional, biographical, constrained, national).

% Study how popular-sovereignty readings operate across polities (France's Fifth Republic referenda tradition, Latin American constituent assemblies, US Article V debates) and can compare outcomes across jurisdictions without being bound by any single one.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which a constitutional order can be revised or re-legitimated when institutional interpreters (courts, legislatures) lose alignment with the polity's actual normative commitments, preventing permanent capture of constitutional meaning by any single institution.
% TRANSFER_FUNCTION: Moves ultimate interpretive authority away from courts and legislatures and toward organized popular mobilization capacity; in practice this transfers real power toward whichever faction can most effectively mobilize mass participation (referenda turnout, convention delegates, sustained movement pressure) and away from those who rely on stable doctrinal protection, including numerical minorities.
% ABSENT_VOICES: Minority rights claimants and diffuse, poorly-organized populations lack the mobilization capacity that this reading treats as the legitimate channel for constitutional authority; they are structurally present as payers but have no comparable seat at the table when popular constitutional moments are convened.
% DISAPPEARANCE_RATIONALE: Proponents of the reading argue that without recognized constituent power, courts and legislatures would ossify into unaccountable oligarchy and the constitutional order would lose its claim to democratic legitimacy — the world rearranges toward permanent institutional supremacy. Judicial and administrative institutions argue the opposite: removing this reading would simply return interpretive stability, and normal politics (elections, ordinary amendment) would continue largely unchanged, since revolutionary or convention-based constitutional moments are rare events in most functioning polities.
% FOUNDING_PROBLEM: Historical constitutional founding moments (revolutionary ruptures, post-colonial constitution-making, post-authoritarian transitions) needed a legitimating theory explaining why the new order bound courts and legislatures that did not yet exist when the people acted to create them — constituent power theory supplies that legitimating account and a channel for future re-foundings.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative scholars outside the mobilized movements (e.g. analyses of the French, South African, and Latin American constituent traditions) corroborate that founding moments genuinely required an extra-institutional legitimating theory. However, independent institutional-stability scholars and comparative rule-of-law indices report that invoking constituent power in mature constitutional democracies today functions less to solve a live founding problem and more as a rhetorical resource deployed opportunistically by whichever faction currently commands mobilization capacity — a status the mobilized movements themselves dispute.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate rather than severe: the reading genuinely performs a coordination function (preventing permanent institutional ossification, legitimating necessary founding and re-founding moments) but the beneficiary/victim asymmetry is real — organized mobilization capacity captures interpretive authority at the expense of institutions and unorganized minorities who cannot mobilize comparably. Suppression (0.38) is moderate: there is no formal coercive mechanism forcing courts to accept popular override, but sustained mobilization pressure and delegitimation campaigns function as informal coercion. Theater ratio (0.48) reflects that invocation of 'constituent power' has become substantially rhetorical in mature constitutional orders — used to legitimate factional projects more often than to resolve genuine founding-moment problems, hence the rising trajectory. Accessibility collapse (0.35) is comparatively low: alternative readings (judicial and legislative supremacy) remain fully live and contested, so this is not a constraint that has foreclosed alternatives — it is one live reading among three. Resistance (0.70) is high: judicial institutions, the legal profession, and administrative bodies actively resist the reading's practical application precisely because it threatens their interpretive finality.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of mobilized citizen movements and convention delegates, this reading is coordination in its purest form — the only mechanism by which a captured or ossified constitutional order can be genuinely re-legitimated by its own ultimate source of authority. From the seat of judicial institutions and minority rights claimants, the identical structural claim operates as an ever-present threat: no interpretive settlement, however carefully reasoned or protective of vulnerable minorities, is ever final, because a sufficiently mobilized popular movement can in principle override it. The engine's per-seat computation should register tangled_rope from the analytical seat (genuine coordination function coexisting with asymmetric extraction) while the trapped institutional and powerless minority seats compute closer to snare-like extraction given their inability to exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobilized citizen movements and amendment campaign organizers sit near the beneficiary end of directionality — the reading's entire logic exists to authorize and legitimate their extra-institutional action, and their exit options (mobile — they can choose whether and when to mobilize) put them structurally close to the constraint's benefiting pole. Judicial institutions and administrative technocracy sit near the target end: they are trapped by the reading's logic (a court cannot coherently claim final authority while operating inside a constitutional order whose own legitimating theory denies judicial finality) and cannot exit without abandoning their own source of authority. Minority rights claimants are the most exposed payers: powerless and trapped, they depend on judicial protection precisely because they lack the mobilization capacity that the reading treats as legitimate, so their directionality sits at the extractive extreme even though they are not the reading's intended target — they are collateral to a contest between mobilized majorities and settled institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating constitutional founding and re-founding moments against institutional ossification) was genuinely live at historical constitutional moments — revolutionary foundings, post-colonial constitution-making, post-authoritarian transitions. In mature, stably functioning constitutional democracies, invocation of constituent power theory increasingly serves factional mobilization rather than addressing a live founding-moment problem — this is the mandatrophy signature: the mandate (legitimating necessary re-founding) persists rhetorically even where its founding condition (an actual crisis of institutional legitimacy requiring extra-institutional re-founding) is largely absent. The founding_problem_status is authored as 'contested' rather than 'dead' because genuine constitutional crises (where courts or legislatures have demonstrably captured interpretive authority against the polity's actual normative commitments) do still arise, and the reading's account of legitimate response to such crises remains live and necessary in those cases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_authenticity,
    'When mobilized movements invoke constituent power to override judicial or legislative interpretation, is this genuine exercise of the polity''s constitutive authority, or organized-faction capture of a legitimating vocabulary that lacks any principled limiting condition?',
    'Comparative case analysis distinguishing invocations that occurred during genuine institutional-legitimacy crises (documented by independent rule-of-law and democratic-backsliding indices) from invocations that occurred during ordinary partisan contestation absent any such crisis.',
    'If invocations cluster overwhelmingly around genuine crises, the reading functions closer to rope (rare, necessary coordination); if invocations cluster around ordinary partisan contestation, the reading functions closer to a snare wielded by whichever faction commands current mobilization capacity against institutions and minorities who cannot match it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_power_authenticity, empirical, 'Whether constituent-power invocation tracks genuine crisis or ordinary factional advantage.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is ''the constituent power of the demos'' a coherent, identifiable source of authority distinct from courts and legislatures, or is it always mediated by some organizing institution (a convention, a referendum apparatus, a party) that reintroduces the very institutional gatekeeping this reading claims to transcend?',
    'Structural analysis of every historical instance claimed as an exercise of constituent power: identify whether an institutional gatekeeper (convention rules, referendum administration, mobilization infrastructure) determined which popular expressions counted.',
    'If constituent power always requires institutional mediation to become actionable, the popular-sovereignty reading is less a genuine fourth branch of authority and more a legitimating overlay on whichever institution successfully claims to channel it — sharply narrowing the reading''s claimed independence from courts and legislatures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether constituent power is ever exercised without institutional mediation.').

omega_variable(
    minority_protection_tradeoff,
    'Does subordinating judicial rights-protection to eventual popular override systematically disadvantage minorities who cannot achieve majoritarian mobilization, even when the reading is applied in good faith?',
    'Cross-national comparison of minority rights outcomes in jurisdictions with strong popular-sovereignty traditions (frequent referenda, easy amendment) versus jurisdictions with strong judicial supremacy traditions, controlling for other institutional variables.',
    'A robust negative correlation would establish minority_rights_claimants as structural, not incidental, victims of this reading — strengthening the tangled_rope classification''s victim leg beyond contingent historical cases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_tradeoff, empirical, 'Whether popular-sovereignty regimes systematically underprotect minorities relative to judicial-supremacy regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__popular_sovereignty_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(cons_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(cons_be_t60, constitutional_text__popular_sovereignty_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(cons_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement(cons_su_t60, constitutional_text__popular_sovereignty_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'ultimate constitutional interpretive authority' per the ε-invariance principle. judicial_supremacy_reading treats courts as final; legislative_sovereignty_reading treats parliament as final (subject to override mechanisms); popular_sovereignty_reading (this story) treats both as subordinate to extra-institutional constituent power. Each has a distinct ε, distinct beneficiary/victim structure, and distinct classification — they are not the same constraint viewed from different angles. Network edges are declared bidirectionally in spirit: this reading's mobilization dynamics create downstream legitimacy pressure on both sibling readings whenever popular movements successfully invoke constituent power to override a judicial or legislative settlement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
