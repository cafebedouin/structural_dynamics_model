% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity as Protective Scaffold for Vigorous Enforcement (Protective Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the qualified_immunity_doctrine
 *   kernel: the protective_scaffold_reading, under which the doctrine is a
 *   necessary protection enabling vigorous law enforcement without fear of
 *   bad-faith litigation. Per the epsilon-referent rule, extractiveness is
 *   authored for the STANDING arrangement under contest — the
 *   qualified-immunity regime as it actually operates — assessed by this
 *   reading's own lights: the reading concedes that judicial discretion in
 *   the 'clearly established' inquiry externalizes real litigation and remedy
 *   costs onto violation survivors (hence moderate, not negligible, epsilon),
 *   while maintaining that the protection performs a genuine filtering and
 *   de-chilling function. The sibling readings — accountability_void_reading
 *   (systematic impunity extraction) and constitutional_fidelity_reading
 *   (judicial fabrication lacking authorization) — are separate constraint
 *   files with their own epsilon, beneficiary/victim structures, and
 *   classifications; they are linked through network edges, not folded into
 *   this one. KEY AGENTS (by structural relationship): police_officers:
 *   primary beneficiary (organized/mobile) — collects personal-liability
 *   protection on every discretionary act; police_unions: organized
 *   defender-beneficiary (organized/mobile) — litigates and lobbies to
 *   preserve the shield; municipalities: derivative beneficiary with residual
 *   exposure (institutional/constrained) — funds indemnity, gains reduced
 *   suit volume; federal_judiciary: agenda-setter
 *   (institutional/identity_locked) — administers the doctrine it authored
 *   and is invested in its stability; constitutional_violation_survivors:
 *   primary target (powerless/trapped) — bears denied remedy and lost suits;
 *   civil_rights_plaintiffs_bar: cost-bearing intermediary
 *   (moderate/constrained) — absorbs contingency risk on cases the doctrine
 *   renders unpredictable; policed_communities_without_remedy: excluded voice
 *   (powerless/trapped) — bears practices no individual suit ever reaches;
 *   legal_scholars: analytical observer (analytical/analytical) — maps the
 *   doctrine's operation from outside.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.52).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.64).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity as Protective Scaffold for Vigorous Enforcement (Protective Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'a994e329-225b-4de3-b5b0-54678561ec56').
narrative_ontology:cs_kernel_codification('a994e329-225b-4de3-b5b0-54678561ec56', fixed_text).
narrative_ontology:cs_authority_grounding('a994e329-225b-4de3-b5b0-54678561ec56', lineage).
narrative_ontology:cs_interpretation_layer_present('a994e329-225b-4de3-b5b0-54678561ec56').
narrative_ontology:cs_reading_relation('a994e329-225b-4de3-b5b0-54678561ec56', qualified_immunity_doctrine__accountability_void_reading, influences).
narrative_ontology:cs_reading_relation('a994e329-225b-4de3-b5b0-54678561ec56', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('a994e329-225b-4de3-b5b0-54678561ec56', foundational, official_immunity_preserves_vigorous_enforcement).
narrative_ontology:cs_axiom_status(official_immunity_preserves_vigorous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a994e329-225b-4de3-b5b0-54678561ec56', official_immunity_preserves_vigorous_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('a994e329-225b-4de3-b5b0-54678561ec56', secondary, personal_ruin_for_reasonable_mistakes_is_unjust).
narrative_ontology:cs_axiom_status(personal_ruin_for_reasonable_mistakes_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('a994e329-225b-4de3-b5b0-54678561ec56', personal_ruin_for_reasonable_mistakes_is_unjust, deontological).
narrative_ontology:cs_reference_frame('a994e329-225b-4de3-b5b0-54678561ec56', calibrated_good_faith_protection).
narrative_ontology:cs_drift_state('a994e329-225b-4de3-b5b0-54678561ec56', contemporary_reform_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a994e329-225b-4de3-b5b0-54678561ec56', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_unions).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipalities).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, municipalities).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs_bar).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, chilling_effect_hypothesis).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, objective_good_faith_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce law in split-second discretionary settings. The doctrine shields them from personal damages awards for constitutional violations unless existing precedent clearly forbade the conduct; they carry essentially no financial exposure in the run of suits and are additionally indemnified by employers in nearly all jurisdictions. Union representation litigates to keep the shield intact. An officer can leave public employment, but the shield follows them into any enforcement role, so exit neither escapes nor threatens the protection.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_officers, beneficiary,
    organized, biographical, mobile, national).

% Collect the protection on behalf of members and spend heavily defending it: amicus appearances in every major immunity case, lobbying against reform bills, campaign support for sympathetic legislators. Dues bases and bargaining posture presuppose the shield. If it fell, their political capital would redirect to other fights without existential loss.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, beneficiary,
    organized, generational, mobile, national).

% Employ the officers and fund indemnification and liability insurance. Individual immunity reduces the volume of successful suits and weakens settlement leverage against their budgets, though direct municipal liability survives for policy-based violations. They bear residual exposure through indemnity payouts and insurance premiums, and their fiscal planning assumes the shield persists. They lobby alongside unions but cannot exit the liability system their employment creates.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipalities, beneficiary,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, municipalities, payer).

% Created the doctrine as common law and administers it case-by-case at summary judgment, deciding whether prior decisions clearly established the violated right. The institution owns the precedent chain and its legitimacy; wholesale retrenchment would concede decades of its own error, so the bench defends the doctrine's framework even as individual judges dissent sharply in particular cases. There is no exit from the role — the doctrine travels with the docket.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Suffered constitutional violations and sued under Section 1983. At summary judgment the court asks whether prior case law clearly established the right; when it finds none, the suit ends without trial, compensation, or factual findings. State tort routes are frequently barred by their own immunity statutes. The injury is already done and the claim has nowhere else to go — exit from the constraint means absorbing the loss.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, national).

% Take constitutional cases largely on contingency. Because prevailing requires locating a factually indistinguishable prior case, outcomes are unpredictable: attorneys decline meritorious-sounding novel claims and absorb unrecoverable hours on the ones they accept. Some have shifted to criminal defense or employment work; the specialty persists but thins, and leaving is a career pivot rather than an escape from the doctrine's operation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs_bar, payer,
    moderate, biographical, constrained, national).

% Live with policing practices that may violate rights but generate no individual lawsuit — no plaintiff, no filing, no record entering the doctrine's machinery. The doctrine's calibration proceeds among courts, institutional defendants, and counsel; these communities appear only as aggregates in use-of-force statistics. They hold no procedural seat and cannot purchase one.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, policed_communities_without_remedy, excluded,
    powerless, generational, trapped, national).

% Document the doctrine's operation — dismissal rates, circuit splits, the granularity of the clearly-established inquiry — and propose reforms from statutory abolition to burden-shifting. They analyze from outside the arrangement; nothing in their professional standing depends on the doctrine surviving or falling, and their output feeds both the reform coalition and the judiciary's self-assessment.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, police_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes personal financial exposure from split-second discretionary decisions by enforcement officers and filters weak suits at summary judgment, so that decision-making is not chilled by fear of ruinous litigation and courts are not flooded by meritless claims.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations — compensation, deterrence signal, and public vindication — away from officers and municipal budgets and onto violation survivors, who retain uncompensated injuries plus unrecoverable litigation costs.
% ABSENT_VOICES: Survivors deterred from filing at all (the dismissal pattern is knowable before suit), community members injured by practices no individual litigant reaches, and future victims of conduct not yet clearly established — none sits at the summary-judgment table where the doctrine's boundaries are drawn; its calibration happens among courts, institutional defendants, and repeat-player counsel.
% DISAPPEARANCE_RATIONALE: Liability insurance pricing, officer training standards, summary-judgment dockets, and municipal indemnification practice all presuppose the doctrine. Overnight removal would reopen the dismissal pipeline, reprice risk across every jurisdiction within a budget cycle, and force renegotiation of union indemnity provisions — the enforcement economy reorganizes around restored personal and municipal exposure.
% FOUNDING_PROBLEM: Protecting good-faith officials from personal financial ruin for reasonable mistakes made while enforcing law during the civil-rights litigation wave, and preventing liability fear from paralyzing discretionary governance.
% FOUNDING_PROBLEM_CORROBORATION: Police unions, municipal insurers, and the administering courts attest the problem is live — but all sit inside or adjacent to the benefiting parties. Corroboration from outside is partial and split: some empirical scholarship reports measurable hesitancy and recruitment effects supporting liveness, while critical legal scholarship and the accountability-void reading find the original gap closed by indemnification and insurance and attest no live protective need. No neutral body attests liveness unambiguously; the statement that outside corroboration is contested is itself the signal.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored moderate (0.52 at interval end) because this reading concedes the 'clearly established' inquiry operates with judge-dependent discretion that denies remedy to some plaintiffs with genuine violations, while holding that the bulk of the doctrine's operation screens weak suits and removes personal financial terror from split-second decisions. Suppression (0.64) is a raw structural property, unscaled by power or scope: the regime's persistence depends on active judicial machinery — summary-judgment dismissal, interlocutory appeal as of right, sequential application of the two-step inquiry — not on participant preference. Theater_ratio (0.33) reflects a growing share of formulaic 'clearly established' research and boilerplate dismissal language layered over a still-real screening function. Accessibility_collapse (0.60): once a plaintiff understands the doctrine, the federal damages route largely closes, though state tort routes survive patchily. Resistance (0.62) is high and sustained — academic criticism, repeated legislative proposals, published dissents, post-2020 reform pressure — yet has not displaced the doctrine. The temporal series run on ONE shared grid (t=0..42, mapping 1982's Harlow baseline to 2024) with all three metrics authored at every point. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change: hardening through the Saucier mandatory-sequencing era and Pearson's sequencing discretion, peaking in the Kisela-era per curiam practice, with slight post-2020 softening as some courts scrutinized egregious facts more closely. Extractiveness rises monotonically then plateaus — rent-layering onto a functioning screen, not collapse. The claimed_type is tangled_rope, stated independently of the metrics: this reading's own structural concessions (a named victim set, enforcement dependence, externalized litigation costs) rule out a pure-coordination claim, while the attested filtering and de-chilling function rules out a pure-extraction claim. The reading's 'scaffold' name is honored as a claim about function, not as a type assertion — the regime carries no sunset clause and its proponents assert permanence, so a scaffold claim would be descriptively false on its face.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the officer and union seats, the arrangement is protection they did not build but depend on: removal exposes members to personal judgments for reasonable mistakes, so the regime reads as necessary coordination. From the survivor seat, the same structure is the machine that ended their case without findings or compensation — extraction experienced at full strength, amplified by trapped exit. The judiciary seat sits between: it experiences the doctrine as its own craft product, costly to repudiate, moderately invested in its persistence. The engine computes this per-seat divergence from the structural data; this story's claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: officers and unions sit near the subsidized end (they collect the shield and bear none of its costs; officers' mobility does not escape the shield, which follows them into any enforcement role). Municipalities are declared beneficiaries but are genuinely dual-positioned — they gain reduced suit volume yet fund indemnification and carry residual Monell exposure — placing them nearer symmetric than the derivation alone would. Survivors derive near-full-target directionality, amplified by trapped exit: the injury is done, the federal route closes at summary judgment, and state routes are frequently barred. The plaintiffs' bar derives moderately high directionality through absorbed contingency risk. Policed communities without individual suits carry diffuse target-position costs with no procedural seat. A single directionality override at the institutional power atom (d=0.35) corrects two agents the derivation mishandles identically: the federal judiciary declares no beneficiary or victim status and would fall to a generic fallback, when its actual position is modest investment in the arrangement's persistence (doctrinal legacy, docket management) without collecting its extraction; and municipalities would derive near-beneficiary values despite their indemnity-borne cost share.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification as tangled_rope prevents two opposite mislabelings. Reading the arrangement as pure coordination (rope) would erase the named victim set — survivors whose remedy the same structure denies — and launder externalized litigation costs as coordination overhead. Reading it as pure extraction (snare) would erase the function this reading attests: merit-suit filtering and de-chilling that courts, unions, and some empirical literature corroborate. The mandatrophy question — whether the doctrine's founding problem (pre-Monell remedial gap, civil-rights-era litigation wave) has outlived its function — is deliberately NOT resolved here: it is routed to the monell_gap_obsolescence omega, because the protective reading holds the problem live while the accountability reading holds it dead, and the resolution is evidentiary, not authorial. If the founding problem resolves dead, the arrangement drifts toward inertial maintenance (theatrical preservation of a rationale nobody needs) or toward capture-flavored extraction; if live, the tangled_rope reading stabilizes. The founding_problem_status is authored 'contested' precisely because the genealogy's liveness is the live dispute between the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the moderate epsilon authored here correctly index the standing qualified-immunity arrangement under the protective reading''s own lights, or do the sibling readings'' values (substantially higher under the accountability-void reading; legitimacy-driven under the constitutional-fidelity reading) describe the same arrangement more accurately?',
    'Compile all three sibling stories of the qualified_immunity_doctrine kernel and compare computed per-seat classifications over the identical structural referent; divergent computed types across readings localize where the readings actually disagree.',
    'If the accountability reading''s higher epsilon dominates across seats, this reading''s moderate epsilon functions as cover-story drift and the family reclassifies toward pure extraction; if this reading''s value computes stably, the protective frame captures real structure the siblings flatten.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'One kernel, three readings: which reading''s epsilon correctly indexes the shared arrangement.').

omega_variable(
    chilling_effect_empirics,
    'Does personal damages liability measurably degrade officer willingness to act decisively in discretionary situations — the empirical premise on which this reading''s entire coordination function rests?',
    'Natural experiments from states that limit immunity or alter indemnification regimes; difference-in-differences designs on use-of-force rates, arrest activity, and hiring before and after immunity-relevant rulings.',
    'A negligible chilling effect collapses the coordination half of the arrangement and pushes classification toward pure extraction; robust effects substantiate the protective function this reading attests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_empirics, empirical, 'Whether the chilling-effect premise is empirically supported.').

omega_variable(
    indemnification_redundancy,
    'Do municipal indemnification and liability insurance mean officers never actually bear the personal financial risk the doctrine shields them from, rendering the protection redundant as protection and pure as rent-shifting?',
    'Indemnification-rate surveys and payout data by jurisdiction; comparison of out-of-pocket officer payments across regimes with differing immunity scope.',
    'Near-universal indemnification converts the protective rationale into insulation without exposure, raising effective extraction and weakening this reading''s core warrant; partial indemnification preserves a residual personal stake supporting the protective frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indemnification_redundancy, empirical, 'Whether the shielded risk is one officers would otherwise bear.').

omega_variable(
    monell_gap_obsolescence,
    'Did Monell v. City of New York (direct municipal liability, 1978) close the structural gap the doctrine originally filled, leaving the protection running without its founding function?',
    'Doctrinal-historical analysis comparing the pre-Monell remedial gap cited in early immunity opinions with the justifications offered in post-Monell applications; trace whether new gaps (e.g., Bivens contraction) reopened a live protective need.',
    'If the founding gap closed and none replaced it, the arrangement persists by inertia and drifts toward theatrical maintenance or pure extraction; if successor gaps are real, the protective function retains live footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monell_gap_obsolescence, empirical, 'Whether the doctrine''s founding remedial gap still exists.').

omega_variable(
    clearly_established_administrability,
    'Is the ''clearly established'' inquiry administrable with tolerable inter-judge variance, or is its discretion the irreducible source of the moderate extraction this reading concedes?',
    'Inter-circuit reversal-rate analysis on immunity summary judgments; within-district variance studies of grant/denial rates across comparable fact patterns.',
    'High variance locates the extraction in unstructured judicial discretion and supports a rising-extraction trajectory; low variance supports this reading''s claim of a calibrated, administrable standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_administrability, empirical, 'Whether the discretion generating extraction is reducible by doctrinal refinement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_protective_scaffold_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qi_protective_scaffold_tr_t6, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement(qi_protective_scaffold_tr_t12, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(qi_protective_scaffold_tr_t18, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(qi_protective_scaffold_tr_t24, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(qi_protective_scaffold_tr_t30, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(qi_protective_scaffold_tr_t36, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(qi_protective_scaffold_tr_t42, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 42, 0.33).

% Extraction over time
narrative_ontology:measurement(qi_protective_scaffold_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(qi_protective_scaffold_be_t6, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement(qi_protective_scaffold_be_t12, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(qi_protective_scaffold_be_t18, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(qi_protective_scaffold_be_t24, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(qi_protective_scaffold_be_t30, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(qi_protective_scaffold_be_t36, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 36, 0.55).
narrative_ontology:measurement(qi_protective_scaffold_be_t42, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 42, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(qi_protective_scaffold_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qi_protective_scaffold_su_t6, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(qi_protective_scaffold_su_t12, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(qi_protective_scaffold_su_t18, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(qi_protective_scaffold_su_t24, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(qi_protective_scaffold_su_t30, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(qi_protective_scaffold_su_t36, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 36, 0.66).
narrative_ontology:measurement(qi_protective_scaffold_su_t42, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 42, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'qualified immunity' decomposes into three structurally distinct constraint stories sharing one kernel (qualified_immunity_doctrine). This story (protective_scaffold_reading) authors moderate epsilon with officers in the beneficiary set and survivors as victims; accountability_void_reading authors high epsilon with the same referent read as impunity machinery; constitutional_fidelity_reading authors extraction independent of policy outcome, keyed to the doctrine's lack of textual authorization. The upstream story (constitutional_fidelity_reading, highest empirical confidence on provenance facts) influences the downstream contest between the two functional readings. Each file links the others via network.affects_constraints; the epsilon differences are the data, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__protective_scaffold_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
