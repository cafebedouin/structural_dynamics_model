% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations — Repudiation Reading (Duress-Illegitimacy Doctrine)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   A defeated state declares the settlement imposed on it void because
 *   consent was obtained under continuing blockade and hunger, and acts on
 *   the declaration: signature under protest in 1919, servicing under protest
 *   through the 1920s, termination at Lausanne in 1932, open default on
 *   external loan service in 1933-34, and remilitarization that converts the
 *   released fiscal capacity into the physical impossibility of collection.
 *   This story instantiates the repudiation_reading of the treaty-text
 *   kernel: the operative arrangement it constitutes — binding obligations
 *   reduced to token gestures while creditor claims stand nullified — is the
 *   standing arrangement under assessment, with ε authored by the reading's
 *   own lights. The sibling readings of the same text are separate constraint
 *   files joined to this one through network edges, each carrying its own ε
 *   and victim set. The claim/metric pairing is deliberate and independent:
 *   the reading CLAIMS the structure as restitution of legitimate
 *   sovereignty, while the authored metrics describe heavily extractive,
 *   increasingly force-maintained operation — the divergence is the datum,
 *   not an error to reconcile. KEY AGENTS (by structural relationship): -
 *   german_reich_government: Agenda-setting beneficiary (institutional /
 *   identity_locked) — administers the repudiation line and collects the
 *   fiscal relief - german_rearmament_industry: Secondary beneficiary
 *   (powerful / constrained) — converts released capacity into procurement
 *   orders - allied_creditor_states: Primary target (institutional / trapped)
 *   — treaty claims nullified without compensation - foreign_bondholders:
 *   Secondary target (organized / trapped) — defaulted loan service, no
 *   collection forum - locarno_guarantee_dependents: Diffuse security payer
 *   (organized / trapped) — absorbs the guarantee architecture's collapse -
 *   german_fulfillment_politicians: Excluded voice (moderate / trapped) —
 *   silenced by the enforcement machinery - international_law_community:
 *   Analytical observer (analytical / analytical) — adjudicates the duress
 *   question without enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.9).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.85).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations — Repudiation Reading (Duress-Illegitimacy Doctrine)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '24c578d4-d766-4423-a6e5-c24706f38437').
narrative_ontology:cs_kernel_codification('24c578d4-d766-4423-a6e5-c24706f38437', fixed_text).
narrative_ontology:cs_authority_grounding('24c578d4-d766-4423-a6e5-c24706f38437', extraction).
narrative_ontology:cs_interpretation_layer_present('24c578d4-d766-4423-a6e5-c24706f38437').
narrative_ontology:cs_reading_relation('24c578d4-d766-4423-a6e5-c24706f38437', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('24c578d4-d766-4423-a6e5-c24706f38437', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('24c578d4-d766-4423-a6e5-c24706f38437', foundational, duress_vitiates_consent_obligations).
narrative_ontology:cs_axiom_status(duress_vitiates_consent_obligations, holdable).
narrative_ontology:cs_axiom_grounding('24c578d4-d766-4423-a6e5-c24706f38437', duress_vitiates_consent_obligations, deontological).
narrative_ontology:cs_axiom('24c578d4-d766-4423-a6e5-c24706f38437', foundational, war_guilt_attribution_is_entente_propaganda).
narrative_ontology:cs_axiom_status(war_guilt_attribution_is_entente_propaganda, holdable).
narrative_ontology:cs_axiom_grounding('24c578d4-d766-4423-a6e5-c24706f38437', war_guilt_attribution_is_entente_propaganda, empirically_contingent).
narrative_ontology:cs_reference_frame('24c578d4-d766-4423-a6e5-c24706f38437', free_consent_settlement_baseline).
narrative_ontology:cs_drift_state('24c578d4-d766-4423-a6e5-c24706f38437', contemporary_jus_cogens_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('24c578d4-d766-4423-a6e5-c24706f38437', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_reich_government).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_rearmament_industry).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, foreign_bondholders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, locarno_guarantee_dependents).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, duress_voids_treaty_binding_force).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, war_guilt_attribution_is_entente_propaganda).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the settlement line: refuses signature of the war-guilt article in 1919, sustains the duress-illegitimacy doctrine as official state teaching, executes the payment stop after Lausanne, and defaults on outstanding external loan service in 1933-34. Receives the entire fiscal relief directly and reallocates it to procurement and autarky programs. Acknowledging any residual binding obligation is framed inside the state's own legitimacy narrative as national surrender, so the stance cannot be revised without revising the regime that holds it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_reich_government, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_reich_government, beneficiary).

% Receives the redirected spending: steel, shipbuilding, chemicals, and munitions firms convert cancelled external transfers into domestic orders. By 1936 their order books and workforce levels depend on the payment stop holding; diversifying away from state procurement would mean losing their dominant customer. The same firms supply the armed forces whose growth backs the default against collection.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_rearmament_industry, beneficiary,
    powerful, generational, constrained, continental).

% Hold the treaty-based claims — the 1921 schedule, later the Young Plan annuities — that the doctrine declares void. Britain, France, Belgium, and Italy attempt collection by occupation in 1923, by restructuring in 1924 and 1929, and by conference in 1932, losing ground at each step; after the defaults of 1933-34 no execution path remains, and absorbing the total loss is the only move left. The same nullified capacity reappears across their borders as military strength.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_states, payer,
    institutional, biographical, trapped, continental).

% Mostly American and Dutch holders of Dawes and Young German external bonds, plus pre-war Reich securities. Coupon streams stop in 1933-34; protective councils negotiate through the decade for fractions of face value against a debtor whose capacity to pay is visibly redeployed into armaments. No court can seize the assets of a sovereign that repudiates behind a growing army, and the securities trade at distressed levels with no recovery mechanism.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, foreign_bondholders, payer,
    organized, biographical, trapped, global).

% Smaller states whose borders and independence rested on the treaty-plus-guarantee system — Belgium, Czechoslovakia, and Poland among them. Each stage of the doctrine's consolidation, from evading the disarmament clauses to remilitarizing the Rhineland in 1936, transfers their security onto their own arsenals. They cannot enforce the abandoned obligations themselves and lack a forum in which their exposure is even priced.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, locarno_guarantee_dependents, payer,
    organized, biographical, trapped, regional).

% Parliamentarians, diplomats, and liberal and social-democratic publics who pursued partial payment under protest to restore sovereignty step by step — the fulfillment line associated with Stresemann. They argue that total rejection forfeits legal continuity, invites isolation, and hands the national narrative to the extreme right. After 1933 they are removed from ministries, universities, and in many cases from public life entirely; the machinery that maintains the doctrine is the same machinery that removes its domestic critics.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_fulfillment_politicians, excluded,
    moderate, biographical, trapped, national).

% Jurists and legal historians who adjudicate, without enforcement power, whether obligations contracted under coercive pressure bind: the 1919 Diktat controversy, the standing of pacta sunt servanda against duress theories, and the codification debates that later culminate in the Vienna Convention's coercion article. They produce the doctrinal record on which every reading of the treaty draws, but hold no lever over any party's conduct.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_law_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, german_reich_government).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a settlement impasse that thirteen years of restructuring had failed to resolve: the reparations schedule was economically untransferable at its assigned scale, and the repudiation frame terminates it unilaterally while giving German domestic politics a single settlement narrative around restored fiscal control and national honor.
% TRANSFER_FUNCTION: Blocks the scheduled transfer of roughly 132 billion gold marks of treaty claims from Germany to the Allied creditor bloc; the cancelled outflows remain in German hands and are redirected after 1933 into armaments procurement and autarky investment. External loan service stops in 1933-34, moving the corresponding loss onto foreign bondholders.
% ABSENT_VOICES: Fulfillment-policy politicians and parliamentary minorities who accepted partial payment under protest are expelled from public life after 1933; foreign bondholders negotiate for years without any collection forum; the smaller guarantee-dependent states learn of the architecture's erosion by fait accompli. Each would contest the settlement frame; none is present when the doctrine consolidates.
% DISAPPEARANCE_RATIONALE: If the repudiation stance vanished overnight — if the German state acknowledged binding residual obligations — the fiscal arithmetic of German rearmament collapses, since procurement was financed from cancelled debt service; the creditor claims revive as live negotiating objects; and the domestic legitimacy narrative built on the imposed-settlement grievance loses its foundation. The interwar settlement economy would reorganize around renewed payment questions rather than around faits accomplis.
% FOUNDING_PROBLEM: Built to escape a reparations burden judged unpayable without national collapse, and to overturn the war-guilt article whose moral attribution underwrote the payment demand — the founding grievance was the combination of fiscal impossibility and imposed humiliation.
% FOUNDING_PROBLEM_CORROBORATION: The fiscal-impossibility half is corroborated from outside the beneficiary coalition: Keynes's 1919 analysis demonstrated the transfer arithmetic, and the successive Dawes and Young committees effectively conceded it by restructuring the schedule twice. The illegitimacy half — that duress voids the treaty — finds essentially no independent corroboration: Allied jurists and most neutral international lawyers treated the treaty as validly concluded under then-operative practice, and no external authority endorsed the reading while it was live. The corroboration asymmetry is itself signal.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.9, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored over the operative settlement this reading constitutes — creditor claims standing nullified while the cancelling party retains and redirects the released flows — measured by the reading's own lights: the reading regards the original treaty as illegitimate imposition and consistently rates the resulting settlement maximally extractive toward the dispossessed creditor seats (0.90 at interval end). Suppression is authored as a raw structural property and is not scaled by power or scope: the series tracks the enforcement machinery itself (occupation-era mobilization, Gleichschaltung, Wehrmacht-backed default), never a contextual modifier. Theater follows an inverted arc: the doctrine spends 1919-1929 as predominantly performative politics — tribunal rhetoric, war-guilt propaganda, Diktat framing — while obligations are in fact serviced under protest; after Lausanne and the 1933-34 defaults the argumentative layer thins because force has replaced it (0.55 falling to 0.22). Accessibility_collapse (0.66) records a two-sided closure: creditor-side collection alternatives collapse nearly completely by 1936, while German-side alternatives (fulfillment policy, negotiated reduction) are foreclosed politically rather than logically. Resistance (0.50) records real but ultimately futile counter-moves — the Ruhr occupation, bondholder litigation, parliamentary opposition — extinguished rather than accommodated. The 1923-1926 oscillation in the suppression series is externally driven (occupation shock, then stabilization), not an intermittent-reinforcement mechanism; the base_properties scalars report the interval-end state, matching the final shared-grid measurements.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the german_reich_government seat the arrangement is recovered sovereignty — the state experiences the doctrine as the removal of an impost, with identity-lock anchoring it at the beneficiary pole regardless of fiscal arithmetic. From the allied_creditor_states and foreign_bondholders seats the same structure operates as total dispossession of claims that carried formal legal standing: trapped exit places both near the full-target end, and scope scaling (continental and global verification difficulty) raises their effective burden further. The guarantee-dependent seats experience the arrangement as a security transfer they never agreed to and cannot price in any forum. The excluded domestic seats would compute yet another position — silenced participants inside the enforcing polity — but their absence is commentary-grade, not correction-grade.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the German seats toward the subsidy end: the government receives the cancelled outflows directly and the industry receives their procurement conversion, neither bearing offsetting costs. Victim declarations drive the three payer seats toward the full-target end, with trapped exit — no collection forum, no court execution against a sovereign backed by growing force, no enforceable guarantee — removing arbitrage-grade relief. The excluded domestic minority derives high d through cost-bearing without compensating flow: it pays in silenced voice rather than money. The observer seat carries analytical d and no material position. No directionality overrides were needed: the beneficiary/victim declarations plus the exit atoms reproduce the intended structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unpayable schedule welded to an imposed humiliation — was substantially resolved by 1932: Lausanne terminated what even the restructuring committees had conceded was economically untransferable. The arrangement nonetheless persists past that function, hardening into enforcement-for-rearmament: the relief is retained, the claims stay dead, and the machinery turns from debt politics to force maintenance. Authored founding_problem_status 'contested' crossed with disappearance_verdict 'world_rearranges' flags precisely this zombie tendency for the mismatch consumer. The tangled_rope claim guards against the opposite mislabels as well: reading the doctrine as pure villainy erases the externally corroborated fiscal substrate (Keynes's transfer arithmetic, two rounds of committee restructuring that implicitly conceded unpayability), and reading it as pure justice-restoration erases the total, uncompensated dispossession its operation visits on the creditor, bondholder, and guarantee-dependent seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_delta,
    'This constraint is one reading (repudiation_reading) of the versailles_reparations_clauses kernel — what would each sibling reading change structurally?',
    'Compare the compiled sibling files: punitive_liability_reading instantiates an operative arrangement whose extraction surface flips to German taxpayers and whose victim set becomes the German population; limited_responsibility_reading instantiates capacity-bounded payments where both blocs retain partial claims. Each sibling carries its own ε referent, victim set, and classification over the same treaty text.',
    'Under the punitive sibling the directionality map inverts and the receipt seat moves to the Allied treasuries; under the limited sibling extraction is bounded by viability tests and both sides keep negotiating leverage. Classification, stakeholder roles, and gain_flow all shift with the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_delta, conceptual, 'Reading-index identity: ε, victims, and type are properties of this reading, not of the treaty label.').

omega_variable(
    consent_under_defeat_semantics,
    'Was German consent to the treaty coerced in the legally operative sense (blockade and hunger pressure continuing through June 1919), or does defeat itself constitute standard war termination within then-prevailing practice, making the treaty validly concluded?',
    'Archival reconstruction of the armistice-extension negotiations and comparative treatment of the other defeated powers'' settlements (Saint-Germain, Trianon, Neuilly) under contemporaneous legal norms.',
    'If coercion was legally operative, the foundational duress axiom gains factual footing and the reading''s ε assessment acquires its justification structure; if the treaty falls within normal practice, the premise is special pleading and the reading''s moral architecture loses its load-bearing wall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_defeat_semantics, empirical, 'Whether the duress premise describes real coercion or reframes ordinary defeat.').

omega_variable(
    vienna_codification_retroactivity,
    'Does the later conventional codification of the duress-void principle (Vienna Convention on the Law of Treaties, coercion article, and the wider jus cogens development) retroactively validate this reading, or does it apply only prospectively to treaties concluded after entry into force?',
    'Doctrinal analysis of the VCLT drafting records and of state practice on applying peremptory norms to pre-existing treaties.',
    'Retroactive validation would rehabilitate the reading''s foundational axiom inside positive international law; prospective-only application leaves the historical reading unsupported by the very principle it anticipated, stranding it as a moral claim without legal warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vienna_codification_retroactivity, conceptual, 'Whether later law reaches backward to legitimize the repudiation stance.').

omega_variable(
    fischer_revision_impact,
    'How does post-Fischer historiography (documented deliberate German war aims from 1961 onward) bear on the reading''s claim that the war-guilt attribution was Entente propaganda rather than fact?',
    'Assess the Fischer thesis and successor scholarship against the reading''s war-guilt-denial claim; trace which revisionist institutions absorbed, suppressed, or ignored the finding after 1961.',
    'If German leadership deliberately sought continental war, the innocent-victim frame collapses, the empirical axiom is overridden within its own evidential base, and the deontological duress axiom it props up stands alone — raising the computed foreclosure exposure of the reading as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fischer_revision_impact, empirical, 'Status of the war-guilt-denial axiom under modern historiography.').

omega_variable(
    persistence_mechanism_split,
    'Is the doctrine''s persistence driven by structural enforcement (state coercion against domestic and creditor alternatives) or by identity fusion (national honor making any acknowledgment unthinkable even absent coercion)?',
    'Counterfactual analysis across enforcement regimes: the German state''s voluntary completion of the last outstanding bond interest in 2010, once regime legitimacy and enforcement stakes had dissolved, suggests the honor frame decayed with its institutional carrier.',
    'If identity-driven, effective suppression persists beyond the enforcement machinery and the German seat''s exit atom is misread by structural derivation alone; if structural, the constraint dies with its enforcer and the identity-lock attribution is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_mechanism_split, empirical, 'Structural versus internalized persistence of the repudiation stance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_repud_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(versailles_repud_tr_t4, versailles_reparations_clauses__repudiation_reading, theater_ratio, 4, 0.62).
narrative_ontology:measurement(versailles_repud_tr_t7, versailles_reparations_clauses__repudiation_reading, theater_ratio, 7, 0.66).
narrative_ontology:measurement(versailles_repud_tr_t10, versailles_reparations_clauses__repudiation_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(versailles_repud_tr_t13, versailles_reparations_clauses__repudiation_reading, theater_ratio, 13, 0.47).
narrative_ontology:measurement(versailles_repud_tr_t14, versailles_reparations_clauses__repudiation_reading, theater_ratio, 14, 0.36).
narrative_ontology:measurement(versailles_repud_tr_t17, versailles_reparations_clauses__repudiation_reading, theater_ratio, 17, 0.27).
narrative_ontology:measurement(versailles_repud_tr_t20, versailles_reparations_clauses__repudiation_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(versailles_repud_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(versailles_repud_be_t4, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(versailles_repud_be_t7, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 7, 0.41).
narrative_ontology:measurement(versailles_repud_be_t10, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(versailles_repud_be_t13, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 13, 0.72).
narrative_ontology:measurement(versailles_repud_be_t14, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 14, 0.81).
narrative_ontology:measurement(versailles_repud_be_t17, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 17, 0.87).
narrative_ontology:measurement(versailles_repud_be_t20, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 20, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(versailles_repud_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(versailles_repud_su_t4, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(versailles_repud_su_t7, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 7, 0.4).
narrative_ontology:measurement(versailles_repud_su_t10, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(versailles_repud_su_t13, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 13, 0.52).
narrative_ontology:measurement(versailles_repud_su_t14, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 14, 0.7).
narrative_ontology:measurement(versailles_repud_su_t17, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 17, 0.79).
narrative_ontology:measurement(versailles_repud_su_t20, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'Versailles reparations' covers three structurally distinct operative arrangements generated from one fixed text. The punitive sibling emits a constraint whose victims are German taxpayers and whose beneficiaries are the Allied treasuries; the limited sibling emits a capacity-bounded settlement with claims preserved on both sides; this repudiation story emits an arrangement whose victims are the creditor, bondholder, and guarantee-dependent seats and whose gains accrue to the German state. The upstream punitive reading is the one whose existence the repudiation reading exists to negate — its liability claim is the object the duress doctrine voids — which is why the family edges run between all three members rather than pairwise in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
