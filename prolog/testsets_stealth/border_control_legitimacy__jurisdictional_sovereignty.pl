% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control Legitimacy under Jurisdictional Sovereignty (Balanced-Admission Reading)
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   This story instantiates the jurisdictional_sovereignty reading of the
 *   border_control_legitimacy kernel: sovereignty is the power to regulate
 *   rights and obligations within territory, which does not by itself entail
 *   authority to close borders, and border governance is legitimate only when
 *   it balances protection obligations against labor needs and public
 *   consent, with enforcement constrained by proportionality and necessity
 *   tests. The standing arrangement under assessment is therefore a
 *   managed-admission regime: states run asylum systems, visa ladders, and
 *   removal machinery, courts police the balance, and legitimacy crises fire
 *   in both directions (enforcement that violates basic rights, or admission
 *   that outruns consent). The reading's distinctive structural feature is
 *   its dual victim set: the balance fails refused protection seekers on one
 *   side and displaced citizen groups on the other, depending on which term
 *   yields. CONSTRAINT FAMILY NOTE: the colloquial label 'border control
 *   legitimacy' decomposes into three structurally distinct constraints
 *   corresponding to the three readings of the kernel. The
 *   sovereignty_primary sibling authors a higher epsilon over an arrangement
 *   with a single victim set (migrants) and no proportionality brake; the
 *   freedom_of_movement_primary sibling authors the standing arrangement as
 *   wholesale illegitimate. This story authors a moderate-high epsilon (0.64)
 *   over the balanced arrangement with dual victims. The epsilon values
 *   differ because the arrangements differ, not because one constraint is
 *   viewed from different angles; the stories are linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - receiving_state_governments: Agenda setter (institutional/constrained) - legislates admission categories, administers asylum and removal, negotiates externalization deals; bound by courts applying the proportionality tests this reading installs
 *   - - receiving_publics: Primary beneficiary with payer overlay (organized/identity_locked) - consumes order, wage-floor protection, and consent-maintained admission levels; funds the enforcement apparatus through taxation
 *   - - licensed_sector_employers: Secondary beneficiary (powerful/mobile) - accesses managed labor supply through visa schemes; retains arbitrage-grade exit via relocation
 *   - - refused_protection_seekers: Primary target (powerless/trapped) - bears denial of protection, detention, and diversion into irregular routes; structurally absent from the design conversation
 *   - - displaced_citizen_groups: Second target (moderate/constrained) - absorbs labor-market and service-strain consequences of admission outcomes; their consent is invoked but aggregated away
 *   - - externalized_transit_states: Dual-positioned intermediary (institutional/constrained) - paid to intercept and host; gains revenue and leverage while bearing hosting burdens and rights exposure
 *   - - smuggling_networks: Parasitic beneficiary (organized/mobile) - captures restriction rents by selling passage the enforcement apparatus makes scarce and dangerous
 *   - - international_human_rights_bodies: Analytical observer (institutional/analytical) - adjudicate proportionality and necessity challenges; hold the constraint's own legitimacy tests against its operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.64).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.68).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.64).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control Legitimacy under Jurisdictional Sovereignty (Balanced-Admission Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '962ad321-88bd-49ed-8724-a62df01bac6c').
narrative_ontology:cs_kernel_codification('962ad321-88bd-49ed-8724-a62df01bac6c', formalized).
narrative_ontology:cs_authority_grounding('962ad321-88bd-49ed-8724-a62df01bac6c', lineage).
narrative_ontology:cs_interpretation_layer_present('962ad321-88bd-49ed-8724-a62df01bac6c').
narrative_ontology:cs_reading_relation('962ad321-88bd-49ed-8724-a62df01bac6c', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('962ad321-88bd-49ed-8724-a62df01bac6c', border_control_legitimacy__freedom_of_movement_primary, influences).
narrative_ontology:cs_axiom('962ad321-88bd-49ed-8724-a62df01bac6c', foundational, border_closure_not_constitutive_of_sovereignty).
narrative_ontology:cs_axiom_status(border_closure_not_constitutive_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('962ad321-88bd-49ed-8724-a62df01bac6c', border_closure_not_constitutive_of_sovereignty, conventional).
narrative_ontology:cs_axiom('962ad321-88bd-49ed-8724-a62df01bac6c', foundational, legitimacy_requires_proportional_balancing).
narrative_ontology:cs_axiom_status(legitimacy_requires_proportional_balancing, holdable).
narrative_ontology:cs_axiom_grounding('962ad321-88bd-49ed-8724-a62df01bac6c', legitimacy_requires_proportional_balancing, deontological).
narrative_ontology:cs_reference_frame('962ad321-88bd-49ed-8724-a62df01bac6c', balanced_jurisdictional_authority).
narrative_ontology:cs_drift_state('962ad321-88bd-49ed-8724-a62df01bac6c', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('962ad321-88bd-49ed-8724-a62df01bac6c', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, receiving_publics).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, licensed_sector_employers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, refused_protection_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizen_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, externalized_transit_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, smuggling_networks).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, receiving_publics).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, externalized_transit_states).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_review_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, necessity_test_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, non_refoulement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate admission categories, run asylum adjudication and removal systems, negotiate externalization agreements with transit states, and deploy enforcement at borders. They are bound by courts applying the proportionality and necessity tests this reading installs, and they cannot walk away from the legitimacy demands without triggering a consent crisis on one side or a rights crisis on the other. Their flexibility lies in choosing which term of the balance to sacrifice and where to locate the coercion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_governments, agenda_setter,
    institutional, generational, constrained, global).

% Receive the arrangement's principal goods: maintained public order, wage-floor protection in exposed sectors, and admission levels that track expressed consent. They fund the enforcement apparatus through taxation and police the balance electorally. Membership in the receiving polity is constitutive of their position; exit means emigration and renunciation, which almost none take, so their relationship to the constraint is fused with national belonging.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_publics, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, receiving_publics, payer).

% Access managed labor supply through visa ladders and quota schemes calibrated to sector demand. They lobby for expansion when labor is scarce and accept restriction when it disciplines wages. Because production is relocatable across jurisdictions, they hold arbitrage-grade exit: they capture the benefit of orderly admission while retaining the option to leave the arrangement's costs behind.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, licensed_sector_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Have their protection claims adjudicated under the balance; refusal routes them into detention, return to unsafe origin countries, or irregular channels where the enforcement apparatus has made passage scarce and dangerous. They bear the arrangement's sharpest costs and are structurally absent from its design: they appear in the process only as individual cases, never as participants in setting the rules, and every geographic route out passes through some state's enforcement reach.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, refused_protection_seekers, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, refused_protection_seekers, excluded).

% Citizens in labor-market competition with admitted workers and in regions where services strain under arrival volumes. They absorb the citizen-side consequences of admission outcomes; their consent is the term of the balance invoked on their behalf, but it is aggregated into national majorities that may not reflect their local situation. Exit means internal migration toward opportunity, which is costly and unevenly available.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizen_groups, payer,
    moderate, biographical, constrained, national).

% Accept payments, trade concessions, and diplomatic leverage in exchange for intercepting departures, hosting processed cohorts, or running third-country screening. They gain revenue and bargaining power while bearing hosting burdens, domestic instability risk, and human-rights exposure before international bodies. Exiting the partnership risks losing the transfers and confronting arrival surges their own infrastructure cannot absorb.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, externalized_transit_states, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, externalized_transit_states, payer).

% Sell passage across the enforcement frontier; their market exists because the constraint makes lawful routes unavailable and their margins grow with enforcement intensity, since each hardening raises the price of the only remaining passage. They can shift routes and methods faster than enforcement can adapt, so the arrangement's suppressive machinery functions as their barrier to entry.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, smuggling_networks, beneficiary,
    organized, immediate, mobile, global).

% Adjudicate challenges to removals, detention conditions, and interception practices against proportionality and non-refoulement standards. They hold the arrangement's own legitimacy tests against its operation, issue binding or persuasive judgments, and possess no enforcement arm of their own - their constraint on the system runs entirely through domestic compliance and reputational cost.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, receiving_publics).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the interface between territorial jurisdiction and human mobility: matches labor demand with admitted supply, sequences protection claims against reception capacity, and maintains the public consent on which stable admission policy depends. Stated without evaluation: whatever else it does, the arrangement solves a real multi-party allocation problem that no current alternative institution solves at comparable scale.
% TRANSFER_FUNCTION: Moves settlement opportunity and protection security from refused migrants toward admitted entrants and receiving publics; moves enforcement costs onto taxpayers and physical risk onto migrants (dangerous routes, detention) and displaced citizens (labor-market and service strain); moves money from destination treasuries to transit-state partners under externalization deals, and from migrants to smuggling networks as the price of irregular passage.
% ABSENT_VOICES: Refused protection seekers are the paradigmatic absent voice: subject to every determination the system makes, present in its design only through proxy litigants and advocacy organizations acting after the fact. Transit-state populations affected by externalized enforcement lack a seat entirely - the deals are struck between governments. Their objections enter the record chiefly as human-rights litigation brought on their behalf, which is testimony arriving after the rules are written.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, labor markets would reprice around unmanaged inflows and outflows, protection systems would reorganize around first-arrival responsibility, the smuggling economy would collapse or transform as lawful routes opened, externalization payments would cease and transit-state bargains would void, and receiving publics would confront admission questions directly rather than through administrative mediation. Nothing about the underlying displacement pressure would resolve, but every structure currently organizing responses to it would rebuild.
% FOUNDING_PROBLEM: Post-1945 mass displacement and decolonization produced cross-border movement at scales that broke ad hoc admission practice. The 1951 Convention settlement built a protection regime for a defined refugee class while preserving state discretion over ordinary admission - creating the balancing problem between protection obligation and sovereign management that this reading later formalized as a general legitimacy test.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR's global displacement series, ICRC field reporting, and the academic migration literature - all outside the benefiting parties - attest that the founding problem persists at record scale. Migrant-led organizations attest the protection gap from the target side. No serious party disputes that displacement pressure exists; the live dispute is over where the balance sits and who bears its costs.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.64 because the arrangement performs real coordination (matching labor demand to admitted supply, sequencing protection claims against reception capacity) while imposing sharply asymmetric costs: refused seekers forfeit protection and safety, displaced citizens absorb competition and strain, and a growing share of the deterrence burden is pushed onto migrants' bodies via route danger and detention. Suppression is authored at 0.68 as a raw structural property (unscaled by power or scope): carrier sanctions, detention estates, pushback practices, and externalized interception are the machinery that holds the balance in place, though proportionality review caps how far it can harden. Theater_ratio at 0.41 reflects the growing share of enforcement activity that is symbolic (visible deployments, performative toughness, headline removal flights) relative to functional admission management. Accessibility_collapse at 0.45: alternatives persist (other destinations, legal channels, onward movement) but narrow substantially once the enforcement architecture is understood, which is characteristic of a hybrid rather than a natural limit. Resistance at 0.52: sustained litigation, advocacy, route-shifting, and periodic political backlash meet the constraint continuously. The measurement series run on one shared seven-point grid (1951-2024) so every tracked metric is authored at every examined time point; all three series rise monotonically with accelerations at 2001 (securitization) and 2015 (cohort crisis), modeling an enforcement ratchet rather than a cycle: extraction accumulation layered onto a live coordination function, with enforcement capacity hardening at each crisis node.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the agenda-setter seat (governments), the arrangement is a legitimacy-preserving machine it built and must constantly recalibrate; from the trapped payer seat (refused seekers), the same structure is enforced exclusion wearing procedural dress; from the beneficiary-payer publics, it is a bargained compromise they fund and police at the ballot box; from the observer seat (human-rights bodies), it is a hybrid whose own tests are increasingly evaded by externalization. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: receiving_publics sit near the beneficiary end (low d) with a slight upward pull from their tax-funded payer overlay; licensed_sector_employers sit nearest the beneficiary pole because their arbitrage-grade exit lets them capture the labor-supply benefit while dodging the arrangement's long-run costs; refused_protection_seekers sit at the full-target end (high d, amplified by trapped exit and the global scope over which verification of their treatment is weak); displaced_citizen_groups sit moderately high (they bear real costs but retain internal-mobility and political voice); externalized_transit_states derive mixed directionality from their dual beneficiary/payer position; smuggling_networks derive near-beneficiary directionality despite sitting outside the lawful order, because restriction rents flow to them. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships, and the nuances (publics' payer overlay, transit states' dual position) are carried by secondary_role declarations rather than numeric patches.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: displacement pressure stands at record scale, so the mandate has not outlived its function and mandatrophy is not resolved. The classification matters in both directions. Reading the arrangement as pure extraction (snare) would erase the genuine coordination it performs - labor matching, claim sequencing, consent maintenance - which no abolitionist alternative currently replaces at scale; reading it as pure coordination (rope) would erase the documented asymmetry - dual victim sets, rising deterrence costs, restriction rents captured by smugglers - that requires active enforcement to hold. The tangled_rope claim keeps both facts on the table. The rising theater_ratio series is the early-warning signal: if the coordination function continues to atrophy while symbolic enforcement grows, the arrangement drifts toward piton dynamics (performance replacing function), and the metric-substitution detector should be watched on exactly this series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (jurisdictional_sovereignty) of the border_control_legitimacy kernel; which reading governs, and how would the victim set and epsilon change under the sibling readings?',
    'Doctrinal consolidation in apex courts or treaty revision: adoption of the sovereignty_primary reading would shrink the victim set to excluded migrants alone and raise epsilon (absolute discretion removes the proportionality brake); adoption of freedom_of_movement_primary would render the standing arrangement itself illegitimate wholesale rather than partially.',
    'The dual-victim structure, the proportionality constraint on enforcement, and the moderate-high epsilon are all properties of THIS reading; a sibling reading instantiates a different constraint with different beneficiaries, victims, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the border-legitimacy kernel is instantiated changes the constraint''s entire structural profile.').

omega_variable(
    consent_protection_tradeoff,
    'When public consent and protection obligations directly conflict (e.g., a large asylum cohort the public opposes), which term of the balance yields, and does the legitimacy test have a determinate answer?',
    'Comparative case analysis of consent-protection collisions (2015-2016 EU cohort, offshore processing regimes): track which obligation was sacrificed, whether courts upheld the sacrifice, and whether legitimacy crises followed on the sacrificed side.',
    'If protection reliably yields to consent, the balance is asymmetric in operation and effective extraction from refused migrants rises above the authored base; if consent yields, the reading operates closer to its stated form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_protection_tradeoff, conceptual, 'Whether the balancing test has determinate content or resolves by default to whichever term is politically cheaper.').

omega_variable(
    externalized_extraction_visibility,
    'Does externalizing enforcement to transit states and third-country processing reduce the arrangement''s extraction, or relocate it beyond the observation of the courts that apply the proportionality tests?',
    'Trace enforcement outcomes and harm rates across the externalization frontier: compare treatment of intercepted cohorts inside versus outside the adjudicating jurisdiction, using partner-state monitoring data and litigation disclosure.',
    'If harms concentrate beyond the jurisdictional reach of proportionality review, the measured suppression understates the arrangement''s true coercive footprint and the legitimacy test is being satisfied by moving the extraction out of view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_extraction_visibility, empirical, 'Whether externalization launders enforcement beyond the constraint''s own legitimacy tests.').

omega_variable(
    displaced_citizen_harm_attribution,
    'Are the harms borne by displaced citizen groups (labor-market competition, service strain) intrinsic costs of the admission balance, or artifacts of under-provisioned integration and redistribution that the balance could absorb?',
    'Natural experiments from regions with equivalent admission but divergent integration investment: if citizen-side harms track integration spending rather than admission volume, the second victim set is a policy artifact, not a structural cost of the constraint.',
    'If the citizen-side victim set is an artifact, the arrangement''s victim structure simplifies and its coordination function looks purer; if intrinsic, the dual-victim structure is load-bearing and the balance is genuinely torn.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_citizen_harm_attribution, conceptual, 'Whether the second victim set is structural or remediable-by-policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1951, 0.15).
narrative_ontology:measurement_basis(bord_tr_t1951, observed).
narrative_ontology:measurement(bord_tr_t1967, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1967, 0.17).
narrative_ontology:measurement_basis(bord_tr_t1967, observed).
narrative_ontology:measurement(bord_tr_t1985, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(bord_tr_t1985, observed).
narrative_ontology:measurement(bord_tr_t1999, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1999, 0.26).
narrative_ontology:measurement_basis(bord_tr_t1999, observed).
narrative_ontology:measurement(bord_tr_t2001, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2001, 0.31).
narrative_ontology:measurement_basis(bord_tr_t2001, observed).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2015, 0.37).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(bord_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1951, 0.38).
narrative_ontology:measurement_basis(bord_be_t1951, observed).
narrative_ontology:measurement(bord_be_t1967, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement_basis(bord_be_t1967, observed).
narrative_ontology:measurement(bord_be_t1985, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1985, 0.47).
narrative_ontology:measurement_basis(bord_be_t1985, observed).
narrative_ontology:measurement(bord_be_t1999, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1999, 0.52).
narrative_ontology:measurement_basis(bord_be_t1999, observed).
narrative_ontology:measurement(bord_be_t2001, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement_basis(bord_be_t2001, observed).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement_basis(bord_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement_basis(bord_su_t1951, observed).
narrative_ontology:measurement(bord_su_t1967, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1967, 0.34).
narrative_ontology:measurement_basis(bord_su_t1967, observed).
narrative_ontology:measurement(bord_su_t1985, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement_basis(bord_su_t1985, observed).
narrative_ontology:measurement(bord_su_t1999, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1999, 0.49).
narrative_ontology:measurement_basis(bord_su_t1999, observed).
narrative_ontology:measurement(bord_su_t2001, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement_basis(bord_su_t2001, observed).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(bord_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the 'border control legitimacy' label per the epsilon-invariance principle. The colloquial concept conflates three structurally distinct claims: (1) sovereignty_primary - absolute exclusion discretion, single victim set, no proportionality brake, highest epsilon; (2) jurisdictional_sovereignty (this file) - balanced admission under proportionality constraint, dual victim sets, moderate-high epsilon; (3) freedom_of_movement_primary - movement as fundamental right, the standing arrangement itself illegitimate. Each story carries its own epsilon, beneficiaries, victims, and classification; upstream/downstream pressure runs from whichever reading dominates apex-court doctrine to the operating environment of the others. This reading influences the freedom_of_movement_primary sibling (institutionalized proportionality review forces its claims to proceed measure-by-measure rather than categorically) and coexists with the sovereignty_primary sibling (both remain live positions across jurisdictions, e.g., plenary-power doctrine versus proportionality review).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
