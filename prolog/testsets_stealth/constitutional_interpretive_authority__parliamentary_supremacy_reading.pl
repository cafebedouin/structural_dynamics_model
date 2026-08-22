% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading — Legislative Final Interpretive Authority, No Judicial Nullification
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the parliamentary_supremacy_reading of the
 *   constitutional_interpretive_authority kernel: within a single
 *   constitutional order, the elected legislature holds final interpretive
 *   authority and no court may void a parliamentary act. The epsilon referent
 *   is the standing arrangement under contest — that settlement as its own
 *   tradition sees it: democratic finality purchased by the absence of
 *   judicial rights guardianship. The reading solves a real coordination
 *   problem (a single electorally accountable locus of ultimate authority,
 *   avoiding inter-branch constitutional crises) while imposing real costs on
 *   identifiable seats (minorities whose protection is purely political; a
 *   judiciary excluded from the final word; an opposition with no forum for
 *   its constitutional objections). The sibling readings —
 *   judicial_supremacy_reading and coordinate_construction_reading —
 *   instantiate different constraints with different beneficiary and victim
 *   sets; they are separate stories linked through the network, not averaged
 *   into this one. The claim/metric gap is deliberate and independent: the
 *   reading is CLAIMED as tangled_rope (genuine coordination plus asymmetric
 *   extraction through the same structure), and the authored metrics describe
 *   that structure without being tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary and agenda-setter (institutional/arbitrage) — holds final interpretive authority, collects interpretive discretion, can restructure the settlement at will
 *   - political_minorities: Primary target (moderate/constrained) — bear the arrangement's costs directly; protection available only through political victory
 *   - constitutional_judiciary: Secondary target (institutional/trapped) — applies every act, excluded from the final word, its office constituted by the settlement that excludes it
 *   - legislative_opposition: Secondary payer (organized/constrained) — full participation in the process, no forum for its constitutional objections on the losing side
 *   - civil_rights_advocacy_groups: Excluded voice (organized/constrained) — locked out of the judicial forum their arguments presuppose
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — sees the full structure and the contest among readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.58).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.52).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading — Legislative Final Interpretive Authority, No Judicial Nullification").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7').
narrative_ontology:cs_kernel_codification('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', distributed).
narrative_ontology:cs_authority_grounding('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', practice).
narrative_ontology:cs_interpretation_layer_present('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7').
narrative_ontology:cs_reading_relation('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', foundational, elected_legislature_final_interpretive_authority).
narrative_ontology:cs_axiom_status(elected_legislature_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', elected_legislature_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', foundational, electoral_accountability_sufficient_rights_protection).
narrative_ontology:cs_axiom_status(electoral_accountability_sufficient_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', electoral_accountability_sufficient_rights_protection, instrumental).
narrative_ontology:cs_reference_frame('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', sovereign_elected_parliament).
narrative_ontology:cs_drift_state('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', contemporary_qualification_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bbc7b40b-574b-4c73-b7a1-6ec46ddb7bb7', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_opposition).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, political_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the legislative agenda and enacts its governing majority's own reading of constitutional limits through ordinary statutes. Every act it passes takes operative effect with no court able to set it aside, so its interpretation of the constitution becomes the law for as long as it holds office. It can also reshape the settlement itself — creating or abolishing courts, entrenching or repealing rights statutes — since no rule binds its successors. Exit is unnecessary: it sits at the apex of the arrangement it administers and can restructure it at will.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, beneficiary).

% Contests elections and loses legislative votes on constitutional questions. When the governing majority interprets the constitution in ways the opposition believes violate rights or exceed proper power, its only remedy is to win a later election; no court will hear the constitutional objection. It remains inside the system because leaving it means forfeiting the chance to govern, and its procedures, funding, and public standing all depend on continued participation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_opposition, payer,
    organized, biographical, constrained, national).

% Hears cases, interprets statutes, and develops the common law, but must give effect to every act the legislature passes, including acts it may read as violating rights or constitutional principle. Its interpretations stand only until the legislature legislates otherwise, and it has no power to set an act aside. Individual judges occasionally signal in extra-judicial writing or obiter remarks that the settlement might not be beyond reconsideration, but an open assertion of review power would trigger a constitutional crisis the courts cannot win. It cannot leave its position: the settlement defines what a court in this system is.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_judiciary, payer,
    institutional, generational, trapped, national).

% Groups that lose the electoral contest — religious minorities, immigrants, dissident movements, unpopular communities — bear the direct cost of the arrangement: whatever protection they have must be won politically, vote by vote, and an act passed against them by a majority is fully operative against them. They cannot exit the jurisdiction cheaply, and their numbers guarantee they lose majoritarian contests by definition; their recourse is persuasion, coalition-building with larger blocs, and waiting for electoral realignment.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, political_minorities, payer,
    moderate, biographical, constrained, national).

% Campaign, litigate, and lobby for rights protection. Their constitutional arguments have no forum: a court will hear their statutory and common-law claims but cannot entertain the claim that an act itself is unconstitutional. They are inside the political conversation as lobbyists but locked out of the judicial forum their strategy presupposes, which is why they consistently campaign for exactly the arrangements this settlement withholds — entrenched rights, bills of rights, judicial review.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, civil_rights_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% Academic lawyers and comparative constitutionalists who map the settlement, trace its drift, and argue its merits against rival arrangements in other democracies. They bear no costs and collect no benefits from the arrangement's operation; their seat is the analytical one from which the full structure — coordination gains, costs, and the contest among readings — is visible.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, electorally accountable final decision-maker for constitutional disputes: when political actors disagree about what the constitution permits, the arrangement routes the dispute to the legislature, whose resolution is operative law without further appeal. This avoids inter-branch constitutional crises over who decides, eliminates litigation over the validity of primary legislation, and keeps constitutional change tied to electoral cycles.
% TRANSFER_FUNCTION: Moves interpretive authority — and with it the power to define the limits of rights — from courts to the governing legislative majority; and moves the cost of rights protection from judicial guardianship onto political mobilization, so that minorities must win politically what they cannot win judicially. It also moves the grounding of constitutional legitimacy from rights-protection to electoral mandate.
% ABSENT_VOICES: Those whose rights lack majoritarian protection are present only as outvoted voters: their constitutional objections have no institutional forum, so their dissent registers solely as lost elections. Civil-rights advocates are in the political conversation but locked out of the judicial forum their arguments presuppose. Those bound by acts passed today who cannot yet vote have no seat at all — no court stands behind them and they cannot yet ballot.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight — if courts woke with power to void acts — legislative strategy would reorganize around anticipated judicial review, rights litigation would expand immediately, political coalitions would re-form around court appointments and judicial philosophy, and every statute passed under the old settlement would become newly contestable. The constitutional division of labor between the branches would rearrange; nothing about current legislative and judicial practice could persist unchanged.
% FOUNDING_PROBLEM: Where ultimate constitutional authority should sit in a democratic order that rejects both royal prerogative and unelected guardianship: after the settlement of crown-versus-parliament, the question became whether the elected representatives of the people, or an independent judiciary, should have the final word on what the constitution permits — and whether coercive law needs any legitimacy ground beyond the electoral mandate.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: constitutional scholarship across traditions (the continuing Diceyan-versus-rights-constitutionalism debate), comparative constitutional practice (every new democracy must answer the same question), and the judiciary's own occasional signals that the settlement may not be beyond reconsideration all attest the problem remains live. Opposition parties and rights advocates attest it in its shifted form — that the live question is now whether electoral mandate suffices where rights are at stake. No party outside the settlement's beneficiaries attests that the problem is dead.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58 — substantial but bounded: the arrangement transfers rights protection from courts to political mobilization, and minorities pay that transfer every time a majority acts against them, yet electoral accountability caps what a majority can sustainably impose on its own base, and minority coalition power (the designed remedy) damps the burden relative to truly immobile seats. Suppression is 0.52 as a raw structural property (unscaled by the engine): judicial nullification is foreclosed by constitutional convention and political supremacy rather than raw coercion — citizens keep democratic voice, courts keep day-to-day interpretive work, but the alternative arrangement (judicial review) is closed inside the system. Theater ratio is 0.22: the doctrine genuinely governs, though a growing share of its maintenance is declaratory — sovereignty reasserted in speeches and preambles while practice (devolution consent conventions, interpretive duties, international instruments) qualifies it. Accessibility collapse is 0.45: the alternative is visible — comparable democracies run on judicial review, and courts occasionally signal in obiter that the settlement is not beyond reconsideration — but foreclosed within the order. Resistance is 0.5: sustained rights campaigning, academic criticism, and periodic judicial signaling, without existential challenge. All three measurement series share one grid (T=0 approximates the 1885 Diceyan consolidation of the classical doctrine; T=120 approximates the contemporary qualification era around 2005); extraction and enforcement-requirement rise as rights expectations grew and one-party governments used the doctrine against unpopular groups, and theater rises as the formal doctrine is increasingly qualified in practice while formally reasserted.
 *
 * PERSPECTIVAL GAP:
 *   The majority seat computes the arrangement as self-government: final authority won at the ballot box, exercised on its own reading of the constitution. The minority and judiciary seats compute the same arrangement as extraction: protection they cannot purchase except politically, and a judicial office defined by applying acts it cannot review. The opposition seat occupies a third position — full participation in the process, zero recourse in its outcomes. The engine computes these per-seat classifications from the structural data; the divergence between them is the measurement, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is declared beneficiary and sits at the beneficiary end of directionality: it collects interpretive discretion without bearing the arrangement's costs, and its arbitrage-grade position (it can restructure the settlement at will) pushes it toward full beneficiary. Political minorities, the judiciary, and the legislative opposition are declared victims and sit toward the target end: minorities bear the costs directly with only constrained exit; the judiciary is trapped — its office is constituted by the settlement it is excluded from; the opposition pays on every lost constitutional contest but retains the biographical hope of becoming the majority, which damps its effective burden relative to the minorities. Civil-rights advocacy groups are excluded rather than coordinated — the judicial forum their work assumes is the closure the enforcement machinery maintains. No directionality overrides are needed: beneficiary/victim declarations plus exit options already place each seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — where ultimate constitutional authority should sit in a democracy that rejects both royal and judicial supremacy — remains live: every proposal to entrench rights or introduce judicial review reopens it, and the arrangement's persistence tracks the problem, not inertia. Mandatrophy is not resolved. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination (rope) would erase the minority and judiciary costs the same structure delivers; reading it as pure extraction (snare) would erase the genuine coordination function — a single accountable final authority — which no rival reading eliminates either, it only relocates. The tangled_rope claim keeps both halves on the books, and the R5 interview corroborates: the founding problem is attested as live from outside the beneficiary set (constitutional scholarship, comparative practice, the judiciary's own occasional signals), so this is not a zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_disagreement,
    'This constraint is one reading (parliamentary_supremacy) of the constitutional_interpretive_authority kernel: would the judicial_supremacy_reading (courts final, acts voidable) or the coordinate_construction_reading (no final authority; inter-branch dialogue) better instantiate the kernel, and where exactly does the disagreement bite?',
    'The disagreement is located in two structural elements: (1) the seat of final authority (legislature vs. courts vs. none) and (2) the sufficiency of electoral mandate as the legitimacy ground for coercive constitutional interpretation. Resolution requires comparative institutional assessment across orders instantiating each reading — rights outcomes, minority protection, constitutional-crisis frequency — which no single test settles; each sibling story carries its own epsilon over the arrangement it instantiates.',
    'Adopting the judicial reading would move the legislature out of the beneficiary set and courts into it, with majoritarian institutions becoming the exposed seat; adopting the coordinate reading would dissolve the single-beneficiary structure entirely and redistribute both coordination credit and cost-bearing across branches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_locus_disagreement, conceptual, 'Which reading of the interpretive-authority kernel this constraint should instantiate.').

omega_variable(
    extraction_intrinsic_vs_settlement_artifact,
    'Is the burden borne by political minorities intrinsic to elected-body final authority as such, or an artifact of this settlement''s specific machinery (plurality elections, dominant lower chamber, unentrenched constitution, no rights-protective second chamber)?',
    'Compare parliamentary-supremacy orders that differ on the machinery — proportionally elected chambers, strong upper houses, statutory bills of rights short of judicial nullification — and test whether minority burden tracks the supremacy doctrine itself or the electoral machinery around it.',
    'If intrinsic, the reading''s hybrid character is stable under any institutional design and the sibling readings are the only escape; if artifact, electoral and procedural reform could cut the measured burden substantially without abandoning the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intrinsic_vs_settlement_artifact, empirical, 'Whether the reading''s asymmetric burden is inherent to majoritarian finality or contingent on settlement design.').

omega_variable(
    suppression_structural_vs_conventional,
    'Is the foreclosure of judicial nullification structural — the courts lack any legal source of review power — or conventional — courts could assert it (as occasional obiter signals) and the assertion would stand if politically tolerated?',
    'Examine judicial assertion episodes and their political reception: a court claim to set aside an act that survived the ensuing political reaction would show the settlement rests on mutual acquiescence rather than absence of power; the observed pattern of obiter signaling without assertion suggests strategic deference, which is conventional.',
    'If conventional, measured suppression overstates the arrangement''s coercive depth — it could dissolve by judicial decision plus political tolerance, without constitutional reconstruction; if structural, only formal reconstruction removes it and the higher suppression reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_conventional, conceptual, 'Whether the bar on judicial review is a hard structure or an enforceable convention.').

omega_variable(
    electoral_mandate_legitimacy_sufficiency,
    'Does the electoral mandate remain a sufficient legitimacy ground for coercive constitutional interpretation as rights expectations rise, or does the legitimacy deficit among the seats bearing the costs grow over the interval?',
    'Longitudinal legitimacy and compliance data following rights-restricting acts passed on thin majorities: whether compliance, political stability, and minority acquiescence hold or decay as the gap between rights expectations and available protection widens.',
    'A widening deficit would push the arrangement''s operation toward burden without consent of the burdened — strengthening the coordinate reading''s case and raising effective suppression; a stable deficit would confirm the mandate''s sufficiency and the reading''s coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_mandate_legitimacy_sufficiency, preference, 'Whether electoral mandate suffices as the legitimacy ground for coercive interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(cons_tr_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(cons_tr_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(cons_tr_t120, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 120, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(cons_be_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(cons_be_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 100, 0.57).
narrative_ontology:measurement(cons_be_t120, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 120, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(cons_su_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement(cons_su_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(cons_su_t120, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 120, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional interpretive authority' decomposes into three epsilon-invariant readings of one kernel. This story (parliamentary supremacy) authors epsilon over the legislature-final arrangement as its own tradition sees it; the judicial_supremacy_reading authors epsilon over court-final rights guardianship with legislative acts voidable; the coordinate_construction_reading authors epsilon over the dialogic arrangement with no final seat. Their epsilon values differ because the arrangements differ — different beneficiary sets (legislature vs. courts vs. distributed), different victim sets, different enforcement structures. The classical settlement (this reading, highest historical continuity) is upstream: it is cited as the established baseline by coordinate-reading proponents and contested by judicial-reading proponents. The readings are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
