% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Colorblind Reading of Equal Protection — Prohibition on State Racial Classification
 *   domain: constitutional law/political philosophy/social policy
 *
 * SUMMARY:
 *   A single colloquial label — 'what equal protection requires' — covers
 *   three structurally distinct legal commitments, and per the
 *   epsilon-invariance principle this file instantiates exactly one of them:
 *   the colorblind reading, in which the Fourteenth Amendment forbids ANY
 *   state use of racial classification, benign or hostile, remedial or
 *   distributive ('Our Constitution is color-blind,' Harlan dissenting,
 *   Plessy 1896). The standing arrangement under contest — and therefore the
 *   referent of epsilon — is race-conscious state action: admissions
 *   preferences, minority set-asides, race-aware districting. Assessed
 *   through this reading's own lights, the classification itself is the harm
 *   regardless of direction or intent, which fixes epsilon in the
 *   moderate-high band (0.42): real opportunity and dignity costs fall on
 *   disfavored-classified individuals, while the arrangement simultaneously
 *   confers genuine access goods on historically excluded groups, keeping it
 *   short of the extreme range. The sibling readings — remedial_reading
 *   (caste-dismantling permits race-conscious measures) and diversity_reading
 *   (race as one factor for educational diversity) — are separate constraint
 *   files with different victim sets and different epsilon values; both are
 *   linked through network.affects_constraints. Within any single
 *   constitutional framework this reading's core premise logically excludes
 *   both siblings' premises, recorded in cs_structure as foreclosure edges;
 *   their historical coexistence across jurisdictions and eras is
 *   jurisdictional patchwork, not logical compatibility. KEY AGENTS (by
 *   structural relationship): - constitutional_judiciary: agenda-setting
 *   interpreter (institutional/analytical) -
 *   civil_rights_enforcement_agencies: agenda-setting enforcer
 *   (institutional/analytical) - state_colorblind_legislatures:
 *   agenda-setting enactor (institutional/arbitrage) -
 *   asian_american_applicants: principal beneficiary (organized/mobile) -
 *   white_applicants_at_selective_institutions: secondary beneficiary
 *   (moderate/mobile) - colorblind_legal_movement: doctrinal beneficiary
 *   (organized/mobile) - underrepresented_minority_applicants: principal
 *   payer (moderate/constrained) - race_conscious_public_institutions: payer
 *   and former administrator (institutional/constrained) -
 *   remedial_reading_proponents: excluded voice (organized/identity_locked) -
 *   comparative_constitutional_observers: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.58).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Colorblind Reading of Equal Protection — Prohibition on State Racial Classification").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional law/political philosophy/social policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'fd545de6-d75d-425b-b574-f6939c2142ab').
narrative_ontology:cs_kernel_codification('fd545de6-d75d-425b-b574-f6939c2142ab', fixed_text).
narrative_ontology:cs_authority_grounding('fd545de6-d75d-425b-b574-f6939c2142ab', lineage).
narrative_ontology:cs_interpretation_layer_present('fd545de6-d75d-425b-b574-f6939c2142ab').
narrative_ontology:cs_reading_relation('fd545de6-d75d-425b-b574-f6939c2142ab', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('fd545de6-d75d-425b-b574-f6939c2142ab', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('fd545de6-d75d-425b-b574-f6939c2142ab', foundational, racial_classification_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classification_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('fd545de6-d75d-425b-b574-f6939c2142ab', racial_classification_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('fd545de6-d75d-425b-b574-f6939c2142ab', secondary, harlan_dissent_authoritative_interpretation).
narrative_ontology:cs_axiom_status(harlan_dissent_authoritative_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('fd545de6-d75d-425b-b574-f6939c2142ab', harlan_dissent_authoritative_interpretation, conventional).
narrative_ontology:cs_reference_frame('fd545de6-d75d-425b-b574-f6939c2142ab', harlan_colorblind_ideal).
narrative_ontology:cs_drift_state('fd545de6-d75d-425b-b574-f6939c2142ab', contemporary_post_sffa_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fd545de6-d75d-425b-b574-f6939c2142ab', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, asian_american_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, white_applicants_at_selective_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_legal_movement).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_public_institutions).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, harlan_colorblind_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, strict_scrutiny_symmetry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Fourteenth Amendment's equal protection guarantee and decides which state uses of racial classification survive review. Struck down race-conscious admissions at Harvard and UNC in 2023, extending a line of rulings from City of Richmond v. Croson through Fisher. Sets the boundary other institutions must comply with; its members bear no personal compliance burden and serve with life tenure.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Federal offices such as the Justice Department's Civil Rights Division and the Education Department's Office for Civil Rights investigate complaints, open compliance reviews, and issue guidance telling universities and agencies how to operate after the 2023 ruling. Career staff build the compliance templates, train investigators, and decide which proxy schemes count as evasion.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, civil_rights_enforcement_agencies, agenda_setter,
    institutional, biographical, analytical, national).

% State legislatures and ballot-initiative sponsors enact their own versions of the rule — California's Proposition 209 (1996), Michigan's Proposal 2 (2006), and a wave of recent statutes restricting race-conscious programming in public institutions. They bind institutions within their borders while remaining free to calibrate exemptions and enforcement budgets as politics shift.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, state_colorblind_legislatures, agenda_setter,
    institutional, biographical, arbitrage, regional).

% High-achieving applicants to selective universities who organized litigation (Students for Fair Admissions) arguing that race-conscious admissions penalized them relative to academic qualifications. After the 2023 ruling, admitted cohorts at several flagged institutions shifted in their favor. They apply widely and can take offers anywhere; their stake is admission odds, not program administration.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_american_applicants, beneficiary,
    organized, biographical, mobile, national).

% Applicants competing for the same seats who were never organized as a constituency but gain relatively wherever race-conscious preferences previously offset academic rankings. Their advantage is diffuse and probabilistic — a few percentage points of admission probability redistributed — and they carry no coordination burden.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, white_applicants_at_selective_institutions, beneficiary,
    moderate, biographical, mobile, national).

% Litigators, scholars, and advocacy organizations who spent decades building the doctrinal case that any official racial classification violates equal protection, culminating in the 2023 ruling. They collect vindication, donor support, and career capital from the doctrine's ascent, and continue litigating its extension to contracting, districting, and corporate programs.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_legal_movement, beneficiary,
    organized, generational, mobile, national).

% Black, Hispanic, and Native applicants whose admission chances at selective institutions declined measurably after California's 1996 ban, Michigan's 2006 ban, and the 2023 national ruling. They cannot exit the admissions system they depend on for upward mobility, and the rule binds every public institution nationally, so choosing another state offers no escape from it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants, payer,
    moderate, biographical, constrained, national).

% Public university systems and state agencies that designed and administered race-conscious programs for decades — admissions preferences, set-aside contracting, targeted outreach — and now must dismantle or redesign them. They set the internal policies the rule operates on, but they no longer control the legal environment: compliance offices rewrite processes, and mission commitments formed over generations collide with the new boundary.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_public_institutions, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, race_conscious_public_institutions, agenda_setter).

% Civil-rights organizations, antisubordination scholars, and jurists who hold that equal protection's purpose is dismantling caste, making race-conscious remedies not merely permitted but required. They testified, published, and litigated throughout the 2010s and 2020s; the 2023 decision placed their reading outside the operative interpretive consensus, and they now argue from opposition. Their professional and ideological identities are fused with the antisubordination project, making abandonment of the position personally unthinkable for many.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, remedial_reading_proponents, excluded,
    organized, generational, identity_locked, national).

% Comparative law scholars and foreign constitutional courts tracking how the United States resolves the tension between formal equality and remedial justice — jurisdictions from India to Brazil made opposite choices on reserved seats and quotas, and the American settlement feeds their debates about which approach better serves multiracial democracies.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, comparative_constitutional_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one administrable bright line — no governmental racial classification — replacing case-by-case adjudication of when racial sorting is acceptable; gives institutions a predictable compliance boundary, gives courts a crisp test, and bars classification in either direction, hostile or remedial.
% TRANSFER_FUNCTION: Moves allocation discretion and admission probability: transfers selective-seat probability mass from applicants aided by racial preferences to applicants ranked higher under race-neutral criteria (measurable in California post-1996, Michigan post-2006, and nationally post-2023); transfers programmatic funds out of race-targeted contracting and outreach; transfers interpretive authority over racial policy to the courts.
% ABSENT_VOICES: Remedial-tradition jurists and civil-rights organizations argued throughout the run-up and were overruled rather than converted; the communities whose remediation the rule forecloses had voice only through those organizations. Going forward, no seat inside the operative consensus represents the position that classification-for-remediation is legitimate — the excluded proponents now speak from litigation and scholarship outside it.
% DISAPPEARANCE_RATIONALE: Repealed overnight, race-conscious admissions would return within an admissions cycle at universities that maintained the infrastructure, agencies would restore set-asides and targeted contracting, admissions compositions would shift back toward pre-2023 patterns, and the enforcement bureaucracy would retool to defend rather than police those programs — the arrangement's absence would be immediately and visibly rearranged by its former administrators and beneficiaries.
% FOUNDING_PROBLEM: Harlan's dissent answered Plessy: the problem was the state sorting citizens into racial castes and stamping one caste inferior. The colorblind reading was built to make any official racial classification constitutionally impossible, so that no government could ever again mark a citizen's race in law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: constitutional historians across traditions date the death of de jure racial caste to Brown v. Board and the 1964 Civil Rights Act; remedial-tradition jurists concede the original target is gone while contending successor subordination keeps remedial tools necessary; even the reading's own advocates cite the solved original problem as grounds for binding everyone symmetrically now. No party claims de jure racial caste persists as law.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on structural grounds independent of the metrics: the rule has a genuine coordination face (one administrable bright line replacing case-by-case racial balancing; it bars hostile classification — exclusionary gerrymandering, discriminatory contracting — as readily as remedial classification), an asymmetric-incidence face (its costs concentrate on actors seeking remediation and on institutions whose missions require race-conscious tools, while its benefits flow to applicants already positioned to win race-neutral competition), and it survives only through active enforcement (court rulings, agency compliance machinery, state statutes). The metrics are authored descriptively, not tuned to the claim. Extractiveness 0.42 sits inside the manifest's moderate-high band because the epsilon referent is the race-conscious arrangement under contest, assessed by this reading's own lights: classification itself is the harm, but the arrangement also delivers real access goods, capping epsilon below the snare range. Suppression 0.58 reflects enforced closure of the race-conscious alternative for every bound institution — a raw structural property the engine leaves unscaled. Theater 0.25: enforcement is now mostly functional, though compliance performance (attestations, audits-as-ritual) persists. Accessibility collapse 0.68: the direct alternative is fully closed; indirect proxies (income, geography, essay signals) remain but are themselves under legal attack. Resistance 0.66: proxy litigation, scholarly contestation, and legislative counter-moves are active though currently losing. Measurement grid maps t to calendar years as year minus 1955 (t0 = 1955, t70 = 2025); all three series share the eight-point grid. Base extractiveness traces the prevalence-and-severity of state racial classification through colorblind eyes: near-maximal in the segregation era (0.85), falling as de jure caste was dismantled, sustaining a plateau through the remedial-expansion decades, then declining as rollback proceeded through the 2023 ruling and its aftermath. Theater ratio falls monotonically (0.75 to 0.25) as the reading moved from pure dissent-symbolism to operative doctrine. Suppression requirement rises monotonically (0.05 to 0.58) as enforcement capacity was deliberately built — an enforcement ratchet, not decay. The extractiveness hump is a secular rise-and-fall, not a cycle: no intermittent-reinforcement mechanism is implicated. Receipt surface: gains are diffuse — each seat was checked, and admission-probability gains scatter across two applicant pools with no organizational capturer, while the legal movement collects status rather than the redistributed opportunity itself; no seat captures the bulk of what the rule's payers surrender. Fixing cost is prohibitive: undoing the rule requires constitutional reinterpretation or amendment against a sitting judicial consensus, a cost dominating any single cohort's benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structural data. From the constitutional_judiciary seat the arrangement is a neutral principle impartially administered — extraction near zero, coordination paramount. From the underrepresented_minority_applicant seat the same rule operates as a barrier foreclosing the remedy for documented disadvantage — high extraction, high suppression. From the asian_american_applicant seat it is restoration of fair competition — closer to pure coordination. From the race_conscious_public_institution seat it is a binding confiscation of policy instruments built over generations — extraction with constrained exit. The engine derives these per-seat classifications from the declared positions and exits; the divergence is the finding, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (asian_american_applicants, white_applicants_at_selective_institutions, colorblind_legal_movement) sit near the beneficiary end of d: the rule subsidizes their competitive position and doctrinal project. Declared victims (underrepresented_minority_applicants, race_conscious_public_institutions) sit near the target end: they bear the transfer of opportunity and policy discretion, with constrained exits — the rule binds every public institution nationally, so relocating within the system buys no escape. Agenda-setting seats (judiciary, enforcement agencies, colorblind legislatures) derive their directionality from administration of the rule rather than payment into it; the judiciary additionally collects authority and legitimacy from the doctrine's success, tilting it further toward the beneficiary end. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct ordering for every seat, and the one dual-positioned agent (race_conscious_public_institutions, payer and former administrator) is handled by secondary_role rather than override. Suppression remains a raw structural input — the engine scales only extractiveness, by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-imposed racial caste — was solved: de jure segregation is legally extinct, a fact corroborated from outside the beneficiary set. The arrangement nonetheless persists and has expanded, redirected from barring hostile classification to barring remedial classification. Authoring founding_problem_status = dead alongside disappearance_verdict = world_rearranges deliberately exposes that mismatch: the doctrine outlived its founding problem and persists by redirection, which is the signature the corpus's zombie/capture flag exists to catch. The tangled_rope claim prevents mislabeling in both directions: reading the rule as pure coordination (rope) would erase the asymmetric incidence its victims document; reading it as pure extraction (snare) would erase the genuine symmetric bright-line function that also bars hostile racial sorting. Mandatrophy resolution here is therefore 'redirected, unresolved': whether the redirected enforcement serves the original protective function or has become an interest-bearing instrument is left to the mandatrophy_redirection_question omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the equal_protection_commitment kernel; would instantiating a sibling reading change the classification?',
    'Doctrinal settlement — a sustained Supreme Court supermajority adopting a sibling''s premise, a constitutional amendment fixing the meaning, or irreversibly consolidated state-level divergence.',
    'Under remedial_reading the victim set becomes the still-subordinated and epsilon indexes caste-perpetuation instead of classification-per-se; under diversity_reading the victim set becomes educational-quality claimants. Either shift moves the computed type — plausibly toward rope (remedial) or a thinner tangled_rope (diversity) — and rewrites the foreclosure edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading-contest over the equal protection kernel: the same clause classifies differently by reading.').

omega_variable(
    classification_harm_magnitude,
    'Through this reading''s own lights, does the harm of race-conscious classification (misallocated opportunity, dignitary injury, eroded trust) outweigh the access goods the arrangement delivers — is 0.42 the right epsilon?',
    'Longitudinal outcome studies exploiting the post-2023 natural experiment and the California/Michigan precedents: admission, completion, earnings, and cross-racial exposure trajectories under race-neutral versus race-conscious regimes.',
    'If measured harm falls well below the goods delivered, epsilon drops toward the rope band and the coordination face dominates; if hidden harms accumulate (mismatch effects, stigma, trust erosion), epsilon climbs toward the snare boundary and the extraction face dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_harm_magnitude, empirical, 'Magnitude of classification harm versus delivered goods under the contested arrangement.').

omega_variable(
    enforcement_symmetry_question,
    'Does the rule bind hostile and remedial classifications with equal vigor in operation, or does it function as one-directional disarmament now that hostile uses are politically extinct?',
    'Audit of enforcement actions and litigation targets since Croson: frequency and intensity of action against majority-favoring or exclusionary classifications versus remedial ones.',
    'If operationally asymmetric, the extraction component grows (the rule disarms only one side''s instruments) and the snare gradient steepens; if genuinely symmetric, the coordination face strengthens and the rope band approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symmetry_question, empirical, 'Operational symmetry of colorblind enforcement across classification directions.').

omega_variable(
    mandatrophy_redirection_question,
    'With the founding problem (de jure caste) dead, does the rule''s continued expansion serve the original protective function (guarding against regression) or has it become an instrument bearing interest for the movements, officeholders, and litigants who advance it?',
    'Counterfactual and follow-the-benefit analysis: would enforcement effort persist if remedial classification vanished entirely; trace career, funding, and electoral returns flowing to enforcement''s champions.',
    'If persistence is purely protective, the constraint drifts back toward rope as its object shrinks; if interest-bearing, theater rises and piton-or-snare drift begins, with the founding-problem mismatch flag hardening into capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_redirection_question, empirical, 'Whether redirected enforcement is protective residue or interest-bearing instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcb_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement_basis(epcb_tr_t0, observed).
narrative_ontology:measurement(epcb_tr_t10, equal_protection_commitment__colorblind_reading, theater_ratio, 10, 0.72).
narrative_ontology:measurement_basis(epcb_tr_t10, observed).
narrative_ontology:measurement(epcb_tr_t20, equal_protection_commitment__colorblind_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement_basis(epcb_tr_t20, observed).
narrative_ontology:measurement(epcb_tr_t30, equal_protection_commitment__colorblind_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement_basis(epcb_tr_t30, observed).
narrative_ontology:measurement(epcb_tr_t40, equal_protection_commitment__colorblind_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(epcb_tr_t40, observed).
narrative_ontology:measurement(epcb_tr_t50, equal_protection_commitment__colorblind_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(epcb_tr_t50, observed).
narrative_ontology:measurement(epcb_tr_t60, equal_protection_commitment__colorblind_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(epcb_tr_t60, observed).
narrative_ontology:measurement(epcb_tr_t70, equal_protection_commitment__colorblind_reading, theater_ratio, 70, 0.25).
narrative_ontology:measurement_basis(epcb_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(epcb_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(epcb_be_t0, observed).
narrative_ontology:measurement(epcb_be_t10, equal_protection_commitment__colorblind_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(epcb_be_t10, observed).
narrative_ontology:measurement(epcb_be_t20, equal_protection_commitment__colorblind_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(epcb_be_t20, observed).
narrative_ontology:measurement(epcb_be_t30, equal_protection_commitment__colorblind_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(epcb_be_t30, observed).
narrative_ontology:measurement(epcb_be_t40, equal_protection_commitment__colorblind_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(epcb_be_t40, observed).
narrative_ontology:measurement(epcb_be_t50, equal_protection_commitment__colorblind_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement_basis(epcb_be_t50, observed).
narrative_ontology:measurement(epcb_be_t60, equal_protection_commitment__colorblind_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement_basis(epcb_be_t60, observed).
narrative_ontology:measurement(epcb_be_t70, equal_protection_commitment__colorblind_reading, base_extractiveness, 70, 0.42).
narrative_ontology:measurement_basis(epcb_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(epcb_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(epcb_su_t0, observed).
narrative_ontology:measurement(epcb_su_t10, equal_protection_commitment__colorblind_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement_basis(epcb_su_t10, observed).
narrative_ontology:measurement(epcb_su_t20, equal_protection_commitment__colorblind_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(epcb_su_t20, observed).
narrative_ontology:measurement(epcb_su_t30, equal_protection_commitment__colorblind_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(epcb_su_t30, observed).
narrative_ontology:measurement(epcb_su_t40, equal_protection_commitment__colorblind_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(epcb_su_t40, observed).
narrative_ontology:measurement(epcb_su_t50, equal_protection_commitment__colorblind_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(epcb_su_t50, observed).
narrative_ontology:measurement(epcb_su_t60, equal_protection_commitment__colorblind_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(epcb_su_t60, observed).
narrative_ontology:measurement(epcb_su_t70, equal_protection_commitment__colorblind_reading, suppression_requirement, 70, 0.58).
narrative_ontology:measurement_basis(epcb_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'equal protection' decomposes into three readings with distinct victim sets and epsilon values. This file (colorblind_reading) holds epsilon 0.42 with race-conscious state programs and their intended beneficiaries in the cost-bearing set; remedial_reading inverts the structure (victims are the still-subordinated; race-conscious measures are the endorsed alternative); diversity_reading occupies the middle (race as one factor, victims are quality-and-fairness claimants). The colorblind reading is upstream in current influence: its 2023 judicial ascendancy stripped the siblings' programs of legal cover, changing their operating environment without resolving the interpretive dispute. Each family member links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
