% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Basic-Law Interpretive Authority
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   A written basic law governs a parliamentary democracy, and under this
 *   reading of it the elected legislature holds the final word on what the
 *   basic law means. Courts interpret the text in the course of adjudication,
 *   but their readings stand only so long as the legislature lets them: an
 *   ordinary act can reverse a constitutional ruling, and the standing threat
 *   of doing so disciplines doctrine between episodes. The arrangement is
 *   justified by democratic mandate — constitutional meaning should answer to
 *   the ballot box — and by representative accountability — officials who
 *   read the basic law wrongly can be removed. The costs land unevenly: the
 *   judiciary absorbs reversals and re-litigation, and groups whose claims
 *   depend on judicial protection must instead win legislative majorities.
 *   Enforcement is active and political: whip discipline, procedural control,
 *   override statutes, and periodic assertions of legislative finality keep
 *   the settlement in place. KEY AGENTS (by structural relationship): -
 *   national_legislature: Primary beneficiary and agenda setter
 *   (institutional/arbitrage) — holds, administers, and can rewrite the
 *   final-word settlement - constitutional_judiciary: Primary target
 *   (institutional/constrained) — rulings reversible at will, bears gridlock
 *   and rework costs - rights_minorities: Secondary target
 *   (powerless/trapped) — protection routes through winning a legislative
 *   majority - electorate: Nominal principal (organized/mobile) — source of
 *   the mandate; gains responsiveness, bears minority-cycle costs -
 *   devolved_regional_assemblies: Excluded seat (organized/constrained) —
 *   would claim a protected interpretive voice, holds none -
 *   comparative_constitutional_scholars: Analytical observer
 *   (analytical/analytical) — maps and critiques the settlement from outside
 *
 * KEY AGENTS:
 *   - national_legislature: primary beneficiary and agenda setter (institutional/arbitrage)
 *   - constitutional_judiciary: primary target (institutional/constrained)
 *   - rights_minorities: secondary target (powerless/trapped)
 *   - electorate: nominal principal (organized/mobile)
 *   - devolved_regional_assemblies: excluded seat (organized/constrained)
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.55).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.6).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Basic-Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "political/constitutional").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73').
narrative_ontology:cs_kernel_codification('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', fixed_text).
narrative_ontology:cs_authority_grounding('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', practice).
narrative_ontology:cs_interpretation_layer_present('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73').
narrative_ontology:cs_reading_relation('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', foundational, elected_representatives_hold_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(elected_representatives_hold_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', elected_representatives_hold_terminal_interpretive_authority, conventional).
narrative_ontology:cs_axiom('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', secondary, representative_accountability_surpasses_insulated_expertise).
narrative_ontology:cs_axiom_status(representative_accountability_surpasses_insulated_expertise, holdable).
narrative_ontology:cs_axiom_grounding('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', representative_accountability_surpasses_insulated_expertise, instrumental).
narrative_ontology:cs_reference_frame('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', democratic_mandate_terminal_authority).
narrative_ontology:cs_drift_state('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', contemporary_rights_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e9d86b64-67ea-44c8-ae57-c0e3f1bf6c73', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electorate).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majoritarian_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, representative_accountability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Debates and enacts statutes that settle contested constitutional questions, and where the framework permits, reverses judicial readings of the basic law by ordinary legislative act. Maintains its final word through procedural control, whip discipline, and periodic sovereignty affirmations. Interpretive authority over the basic law flows to it and stays there; it can also rewrite the settlement itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature, beneficiary).

% Hears challenges to legislation and rules on basic-law meaning, knowing any ruling can be undone by the next session of the legislature. Absorbs the delay and rework of re-litigating questions after overrides, and calibrates doctrine defensively to avoid provoking reversal. Leaving the framework is not available; shaping doctrine within the space the legislature tolerates is the working margin.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_judiciary, payer,
    institutional, generational, constrained, national).

% Depend on legislative majorities to vindicate claims against the state, since judicial victories in their favor can be legislatively erased. When an override lands on their claims, their recourse is persuasion, coalition-building, or endurance; relocating out of the jurisdiction is costly and rarely realistic.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, biographical, trapped, national).

% Supplies the mandate the arrangement runs on: representatives claim authority to settle constitutional meaning because voters chose them. Individual voters gain constitutional meaning that answers to electoral choice, though any voter sits in the minority position on some question in some cycle, and interpretive questions rarely dominate ballot choices.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electorate, beneficiary,
    organized, biographical, mobile, national).

% Govern territories whose competences exist at the center's discretion. They would seek a protected voice in basic-law interpretation — entrenchment of their powers, a referral right, a veto over interpretive settlements — but hold no seat in the arrangement and can only petition the center that defines their authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, devolved_regional_assemblies, excluded,
    organized, generational, constrained, regional).

% Study how different democracies allocate final interpretive authority, publish comparisons and critiques, and advise reform commissions. Neither collects from the arrangement nor bears its costs; their leverage is reputational and advisory.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: When institutions armed with different warrants disagree about what the basic law requires, some rule must terminate the dispute or every disagreement becomes a regime-level crisis. This arrangement supplies that rule: the elected chamber's reading stands, converting open-ended constitutional conflict into an ordinary legislative decision.
% TRANSFER_FUNCTION: Moves final interpretive authority from the courts to the elected legislature; moves the cost of vindicating contested claims onto minorities, who must assemble legislative majorities once judicial protection becomes reversible; moves the delay and rework costs of overridden rulings back onto judicial dockets.
% ABSENT_VOICES: Rights minorities enter the conversation only when a case reaches them, and devolved territorial governments hold no seat at all — both would ask for entrenched protections or referral rights before interpretive settlements harden. Future generations, who inherit the hardened settlements, are present through no one.
% DISAPPEARANCE_RATIONALE: If the legislature's final word vanished overnight, every statute's meaning would reopen as a live judicial question, the government's program would proceed under litigation stays, and dockets would fill with basic-law challenges. The political branches would either accept a new terminal authority or manufacture one — the constitutional order cannot run without some allocation of the last word, so it rearranges rather than dissolves.
% FOUNDING_PROBLEM: Divided claims to final authority over the fundamental law had produced deadlock and crisis: crown, courts, and parliament each asserted the last word, and unsettled sovereignty made ordinary government hostage to institutional standoff. Assigning the last word to the elected chamber was meant to make government possible, correctable, and accountable.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians document the founding settlements and the deadlock they answered; judicial opinions across jurisdictions concede the necessity of some terminal rule even while contesting its locus; comparative scholarship confirms that every working democracy allocates final interpretive authority somewhere. Corroboration comes substantially from outside the beneficiary set — notably from the judiciary itself, a paying party, which attests the problem's liveness while disputing the solution.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.55 because the transfer this arrangement performs is asymmetric but episodic: the standing condition is continuous judicial deference plus minority exposure, while the acute costs land at override moments. Suppression is 0.60 and is a raw structural property, unscaled by power or scope: courts cannot exit the framework, and minorities' alternatives collapse to persuasion and coalition. Theater is 0.42 — the democratic-mandate justification is invoked far more often than interpretive positions are actually consulted at the ballot box, but elections do genuinely constrain, so the performance is partial, not hollow. Accessibility collapse is 0.62: within the framework the alternative (a rival terminal authority) collapses completely once the settlement is understood, because finality is exclusive by construction, while the meta-level choice of settlement remains politically alive, keeping the figure below natural-law territory. Resistance is 0.50: doctrinal self-extension up to the tolerance line, scholarly critique, minority mobilization, and territorial petitions for entrenchment. The claimed type is authored from structure — a real coordination function (a terminal decision rule) joined to enforced asymmetric extraction — and the metrics are authored from observed operation; neither was tuned to the other. All three measurement series share one grid ({0, 8, 16, 24, 32, 40}) and rise together, modeling an enforcement ratchet: as judicial review expanded, override machinery matured, mandate invocations grew more formulaic, and the standing threat came to do more work than realized overrides.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the legislature's position the arrangement is self-government working: the people's representatives settle what the fundamental law requires and can correct mistakes. From the bench the same structure is subordination punctuated by rework: careful doctrine can be erased by a whipped majority, so the rational strategy is self-limitation. From a minority litigant's position the arrangement converts a rights question into a headcount. Same rules, three different lived arrangements — the engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature is declared beneficiary and holds the pen: it collects interpretive authority, faces no rival forum, and can rewrite the settlement itself, placing it near the beneficiary end. The judiciary is declared victim with constrained exit — institutional standing does not shield its rulings from reversal — placing it near the target end. Rights minorities are victims with effectively no exit, sitting nearest the full-target end; their coalition capacity (civil-society alliances, electoral blocs, cross-border publicity) is the main brake on realized extraction and is why suppression, though substantial, stops short of the coercive ceiling. The electorate is declared beneficiary with mobile exit — it can replace the government — but any voter occupies the minority position in some cycle, tempering its subsidy toward symmetry. Devolved assemblies sit outside the settlement entirely; their authored absence is commentary-grade, not a classification input.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating a terminal word so government can proceed — is live, so no mandatrophy is declared. The classification work here is boundary-keeping in both directions: against the pure-extraction reading, the arrangement solves a real collective-action problem no democracy escapes, and its victims are hit episodically rather than continuously milked; against the pure-coordination reading, the extraction is real, enforced, and asymmetric — courts and minorities pay through the same structure that coordinates the branches. Holding both facts in view is what the hybrid category exists for; collapsing either side would falsify the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel basic_law_interpretive_authority (reading: parliamentary_sovereignty_reading). What structural changes would the sibling readings instantiate?',
    'Adopting judicial_supremacy_reading relocates the beneficiary set — courts gain terminal authority while the legislature becomes a constrained actor whose enactments are provisional pending review; adopting popular_constitutionalism_reading dissolves terminality altogether, removing the agenda_setter seat and redistributing interpretive authority across contesting publics.',
    'Classification is reading-relative: the same basic-law text yields this reading''s hybrid coordination-plus-extraction profile, but a different beneficiary/victim geometry under the judicial sibling and no stable seat structure under the popular sibling. Cross-reading comparison requires the separate linked stories, never averaged metrics over one file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: kernel membership, reading instantiation, and the structural deltas sibling readings would produce.').

omega_variable(
    override_frequency_vs_standing_threat,
    'Is the measured extraction driven by realized legislative overrides or by the standing threat that chills judicial doctrine between episodes?',
    'Longitudinal coding of override episodes, reversal statutes, and documented instances of courts narrowing rulings to avoid provocation; compare realized-override rates against doctrinal self-limitation frequency.',
    'If threat-driven, the enforcement machinery extracts more than the override record shows and the arrangement''s suppressive character is understated by episode counts alone; if overrides are rare and benign, the coordination function dominates and the profile shifts toward the coordination pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_frequency_vs_standing_threat, empirical, 'Whether extraction is realized at override moments or extracted continuously via chilling effects.').

omega_variable(
    mandate_specificity_theater,
    'Do voters actually select representatives on basic-law interpretive positions, or is the democratic mandate invoked after the fact to dress policy-driven overrides in accountability language?',
    'Electoral-behavior studies and manifesto coding: measure how often interpretive positions appear in campaigns, move votes, and bind representatives afterward; inspect whip-office records for the deliberative depth preceding overrides.',
    'If the mandate is largely post hoc, the theater ratio is understated and the accountability justification weakens toward cover for majoritarian preference; if voters genuinely sort on interpretive questions, the mandate leg is load-bearing and the arrangement''s legitimacy claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_specificity_theater, empirical, 'Whether the democratic-mandate justification reflects real electoral authorization or retrospective rationalization.').

omega_variable(
    minority_cost_acceptability,
    'Is the exposure of rights minorities to majoritarian override an acceptable price of democratic self-government, or an unjust burden the arrangement imposes on its weakest parties?',
    'Not resolvable by data alone; resolved by constitutional-political value choice — entrenchment decisions, rights-chart adoption, supermajority requirements — taken by the polity itself.',
    'Determines the normative weight assigned to the victim set: the engine computes the extraction symmetrically either way, but the polity''s answer decides whether the minority-bearing component reads as legitimate coordination cost or as a defect demanding structural remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_cost_acceptability, preference, 'Values question underlying the weight of the minority victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.53).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the constitution' decomposes into distinct constraints per reading of the kernel basic_law_interpretive_authority. This file authors the parliamentary_sovereignty_reading only; the judicial_supremacy_reading and popular_constitutionalism_reading are separate stories with their own epsilon values, beneficiary/victim sets, and classifications. Family links run through network.affects_constraints in all three files; the upstream/downstream pressure between them (each reading's adoption changes the legitimacy conditions of the others) is documented in each file's cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
