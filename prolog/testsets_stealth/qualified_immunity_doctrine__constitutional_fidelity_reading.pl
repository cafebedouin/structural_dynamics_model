% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine — Constitutional Fidelity Reading
 *   domain: constitutional law / civil rights / law enforcement policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the qualified_immunity_doctrine
 *   kernel: the constitutional_fidelity_reading, which assesses the standing
 *   arrangement — the judicially created immunity defense to 42 U.S.C. § 1983
 *   — as lacking constitutional or statutory authorization and therefore
 *   illegitimate regardless of policy outcomes. The ε referent throughout is
 *   the standing doctrine as it actually operates (Pierson through the
 *   contemporary clearly-established regime), assessed by this reading's own
 *   lights; it is never the direct-liability regime this reading would
 *   endorse in its place. The reading's distinctive structural claim is that
 *   the concentrated beneficiary is the judiciary itself, which expanded its
 *   own agenda-setting authority over a statute enacted by another branch;
 *   officers and municipalities collect a collateral subsidy while remaining
 *   subjects of an unauthorized standard. Sibling readings —
 *   protective_scaffold_reading (immunity as necessary protection) and
 *   accountability_void_reading (immunity as systematic impunity mechanism) —
 *   are separate constraints with their own ε values, beneficiary sets, and
 *   types; the contest structure is routed to omega variables and the family
 *   is linked via network edges. Claim and metrics are authored
 *   independently: the claimed type states this reading's structural verdict,
 *   the metrics describe observed operation, and the engine computes per-seat
 *   classifications from the structural data without reference to the claim.
 *
 * KEY AGENTS:
 *   - - supreme_court: Agenda-setter and concentrated beneficiary (institutional/identity_locked) — fabricated the defense, refines it, collects the precedential authority; exit would require admitting the doctrine was made rather than found
 *   - - lower_federal_courts: Enforcing intermediaries (institutional/constrained) — gain docket relief, bear the application burden and reversal exposure of an incoherent standard
 *   - - law_enforcement_officers: Collateral beneficiaries and partial subjects (organized/constrained) — collect near-categorical damages protection while being judged by a retrospectively defined standard no legislature wrote
 *   - - municipal_governments: Indirect beneficiaries (powerful/mobile) — Monell-exposed in principle, relieved in practice when officer-level dismissals collapse suits
 *   - - civil_rights_plaintiffs: Primary targets (powerless/trapped) — meritorious claims extinguished at summary judgment; no alternative forum for the federal remedy
 *   - - congress: Displaced authorizer (institutional/constrained) — holds sole formal power to define or abolish the defense, locked out of the operative conversation by procedure
 *   - - state_legislatures: Workaround builders (institutional/constrained, regional) — constructing parallel statutory channels without any seat in the federal doctrine
 *   - - constitutional_scholars: Analytical observers (analytical/analytical) — cross-ideological critical consensus feeding briefs and model statutes, holding no decision seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.74).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.65).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine — Constitutional Fidelity Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional law / civil rights / law enforcement policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '074b8f5a-ea09-451e-b661-783da30cf281').
narrative_ontology:cs_kernel_codification('074b8f5a-ea09-451e-b661-783da30cf281', fixed_text).
narrative_ontology:cs_authority_grounding('074b8f5a-ea09-451e-b661-783da30cf281', lineage).
narrative_ontology:cs_interpretation_layer_present('074b8f5a-ea09-451e-b661-783da30cf281').
narrative_ontology:cs_reading_relation('074b8f5a-ea09-451e-b661-783da30cf281', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('074b8f5a-ea09-451e-b661-783da30cf281', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('074b8f5a-ea09-451e-b661-783da30cf281', foundational, no_immunity_without_statutory_or_constitutional_authorization).
narrative_ontology:cs_axiom_status(no_immunity_without_statutory_or_constitutional_authorization, holdable).
narrative_ontology:cs_axiom_grounding('074b8f5a-ea09-451e-b661-783da30cf281', no_immunity_without_statutory_or_constitutional_authorization, conventional).
narrative_ontology:cs_axiom('074b8f5a-ea09-451e-b661-783da30cf281', foundational, illegitimacy_independent_of_policy_outcomes).
narrative_ontology:cs_axiom_status(illegitimacy_independent_of_policy_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('074b8f5a-ea09-451e-b661-783da30cf281', illegitimacy_independent_of_policy_outcomes, deontological).
narrative_ontology:cs_axiom('074b8f5a-ea09-451e-b661-783da30cf281', secondary, judicial_defense_creation_usurps_legislative_power).
narrative_ontology:cs_axiom_status(judicial_defense_creation_usurps_legislative_power, holdable).
narrative_ontology:cs_axiom_grounding('074b8f5a-ea09-451e-b661-783da30cf281', judicial_defense_creation_usurps_legislative_power, conventional).
narrative_ontology:cs_reference_frame('074b8f5a-ea09-451e-b661-783da30cf281', statutory_text_exclusive_authorization).
narrative_ontology:cs_drift_state('074b8f5a-ea09-451e-b661-783da30cf281', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('074b8f5a-ea09-451e-b661-783da30cf281', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created the immunity defense for officials sued under the 1871 civil-rights statute, first grafting a good-faith defense onto the statute's text (Pierson v. Ray, 1967) and later replacing it with an objective 'clearly established law' standard (Harlow v. Fitzgerald, 1982). Refines the standard case by case, decides which reform petitions to hear, and accumulates the precedential authority that comes from controlling the statute's effective reach. Stepping back would require treating its own prior handiwork as revisable policy rather than discovered law, against an institutional self-image of finding rather than making rules.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court, beneficiary).

% Apply the clearly-established inquiry to thousands of motions each year, frequently ending claims before discovery. They gain docket relief when suits terminate early but carry the burden of an incoherent standard — hypothetical-specificity puzzles, circuit splits, reversal exposure — and cannot decline to apply a defense their reviewing courts maintain.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts, beneficiary).

% Receive near-categorical protection from personal damages liability for on-duty misconduct, with municipal indemnification typically absorbing the residue. At the same time they are judged by a standard no legislature wrote, defined case-by-case after the fact, which shapes training programs, insurance pricing, and personal uncertainty about what conduct will later count as lawful.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, payer).

% Remain fully exposed to liability for their own policies under Monell, but benefit indirectly when private suits collapse at the officer-immunity stage: fewer settlements, smaller indemnity outlays, staffing retained without chill. They fund associations that defend the doctrine and lobby against its legislative repeal.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_governments, beneficiary,
    powerful, generational, mobile, national).

% Bear the cost directly: people with meritorious claims of constitutional violations see their federal damages suits dismissed at summary judgment because the specific right was not 'clearly established,' often without discovery or trial. Their alternative channels — state tort law, state constitutional claims, prospective injunctive relief — are narrower, slower, or unavailable, and they cannot shop for a different forum for the federal remedy the statute promises them.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Enacted the 1871 statute imposing liability on 'every person' and retains sole formal power to define or abolish any defense to it. Bills to codify or eliminate the doctrine have been introduced repeatedly and stalled; bicameralism and Senate procedure keep the body out of the operative conversation even as the doctrine rewrites the statute's effective scope year by year.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, constrained, national).

% A handful — Colorado, New Mexico, and others — have created statutory causes of action that route around the federal defense for state-law claims. They build parallel remedial channels for their residents but hold no seat in the federal doctrine's refinement and cannot restore the federal remedy their residents lost.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, state_legislatures, excluded,
    institutional, generational, constrained, regional).

% Document and criticize the doctrine's development. Objection spans the ideological spectrum — originalist scholars object to the absence of textual and historical foundation, progressive scholars to the resulting impunity — and the near-consensus feeds amicus briefs, model statutes, and genealogical scholarship rather than any decision-making seat.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives federal courts a uniform summary-judgment screen for a high-volume civil-rights docket and gives officials a predictable shield formulation; allocates scarce judicial time away from merits trials on claims the screening standard removes early.
% TRANSFER_FUNCTION: Moves damages remedies away from victims of constitutional violations (claims dismissed before discovery or trial), moves litigation-risk insulation to individual officers and indemnity savings to municipal budgets, and moves agenda-setting authority over the 1871 statute's effective scope from Congress to the federal judiciary.
% ABSENT_VOICES: Congress — the body whose statute the doctrine qualifies — is absent from the operative conversation despite holding the formal power to end it; civil-rights plaintiffs appear only as movants trying to defeat dismissal, never as participants in the standard's design; state legislatures building workarounds have no seat in the federal doctrine's refinement.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, summary-judgment dismissals of damage claims would fall sharply, surviving suits would proceed to discovery and trial, settlement values and liability-insurance pricing would reprice, officer training and supervision incentives would shift toward documented compliance, and Congress would face immediate pressure to legislate an express immunity standard if one were wanted — the remedial economy of Section 1983 would reorganize around the statute's enacted text.
% FOUNDING_PROBLEM: Section 1983 as enacted imposed liability on 'every person' who deprives rights under color of law. Facing a rising volume of suits against officials, the Court constructed a good-faith immunity defense (Pierson v. Ray, 1967), then replaced its subjective anchor with an objective 'clearly established law' standard cutting off discovery (Harlow v. Fitzgerald, 1982) — a defense the statute's text nowhere mentions and no constitutional provision authorizes.
% FOUNDING_PROBLEM_CORROBORATION: No source outside the benefiting parties attests that the founding problem remains live: the 'live' attestation comes from the doctrine's own operators. Outside corroboration runs the other way — dissenting opinions (e.g., Justice Sotomayor's Kisela dissent) document the doctrine's detachment from any remedial purpose, originalist and progressive scholarship alike attest the absence of statutory or historical foundation, libertarian and civil-rights organizations file aligned amicus briefs against it, and legislative hearings on the Ending Qualified Immunity Act record the disputed status on the record.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the doctrine extinguishes the statutory remedy for most meritorious claims before discovery while transferring both remedial value (to protected officials and their indemnitors) and agenda authority (to the Court). Suppression (0.65) reflects active maintenance: interlocutory appeal asymmetry favoring defendants, discovery cutoff, certiorari denial patterns shielding the doctrine from reform cases, and stare decisis invoked selectively. Theater (0.52) crosses the Goodhart threshold honestly: the 'clearly established law' inquiry performs neutral, case-by-case adjudication while operating in practice as a near-categorical dismissal device sustained by specificity games and reliance on unpublished dispositions. Accessibility collapse is low-moderate (0.38) because alternatives genuinely persist — state tort channels, state constitutional claims, prospective relief, and the legislative route — they are merely narrower and slower than the blocked federal damages channel. Resistance (0.62) is unusually high for a judicial doctrine: a cross-ideological scholarly consensus, recurring abolition bills, state-level statutory workarounds, and internal dissenting opinions. The temporal series run on one shared eight-point grid (1967–2025) so every tracked metric is authored at every examined time point; the trajectories show extraction accumulating in steps at the doctrine's construction moments (Harlow 1982, Saucier 2001, Pearson 2009) rather than drifting smoothly.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical doctrine text. From the supreme_court seat the arrangement is disciplined doctrinal housekeeping it authored and maintains; from the civil_rights_plaintiffs seat it is the annihilation of a promised remedy by a rule nobody enacted; from the law_enforcement_officers seat it is simultaneously a shield and an unpredictable trap; from the congress seat it is a standing usurpation of legislative power administered daily without its consent. The excluded seats see no offsetting coordination benefit at all, which is why the same structure supports both a hybrid verdict (from seats inside the arrangement) and a pure-usurpation verdict (from seats outside it). The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: the judiciary seats sit near the beneficiary pole (the Court concentrates the power gain; lower courts share diluted docket relief), officers and municipalities collect the material subsidy, and civil_rights_plaintiffs sit near the full-target pole amplified by trapped exit. Two overrides correct derivations the structural arrays alone would get wrong. First, law_enforcement_officers (organized): the beneficiary listing derives a near-full-beneficiary d, but this reading holds officers are also partial targets — governed by a retrospectively defined standard that imposes real training, insurance, and certainty costs — so d is overridden upward to 0.18. Second, municipal_governments (powerful): beneficiary status plus mobile exit derives an arbitrage-grade near-zero d, but municipalities remain fully exposed to Monell liability and their benefit is purely derivative of officer-level dismissals, so d is overridden upward to 0.22 to reflect their thinner-than-derived subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — a good-faith shield congruent with common-law immunity as it existed in 1871 — was abandoned by the doctrine's own architects when Harlow replaced subjective good faith with an objective clearly-established inquiry severed from any historical anchor; the mandate was not merely outlived but replaced with a different arrangement wearing the old name, so mandatrophy_resolved is declared true. The classification discipline cuts both ways here. Recognizing the thin genuine coordination function (docket management, uniform screening) prevents mislabeling the arrangement as pure extraction with a fake cover story — the function exists even though this reading holds it cannot legitimate anything. Simultaneously, keeping the provenance defect structurally legible (unauthorized creation, concentrated judicial capture, extinguished remedies) prevents the coordination residue from laundering the arrangement as benign coordination. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) records that the parties dispute whether any live problem remains while agreeing the world is organized around the doctrine — the signature of a contested-mandate hybrid rather than a resolved zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical_location,
    'This constraint is one reading of the qualified_immunity_doctrine kernel; which structural element separates the three readings?',
    'Compare the three family stories'' beneficiary sets, epsilon values, and claimed types: constitutional_fidelity locates the defect in authorization (provenance), protective_scaffold in necessity (function), accountability_void in impunity (extraction magnitude).',
    'If the disagreement collapses to a single element — e.g., all three reduce to extraction magnitude — the kernel decomposes into one constraint rather than three; if irreducible, the three-story family stands and cross-reading comparison becomes the primary analytic instrument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexical_location, conceptual, 'Location of the inter-reading disagreement within the qualified immunity kernel.').

omega_variable(
    sibling_delta_protective_scaffold,
    'What would the protective_scaffold_reading change structurally if adopted as the operative reading of the kernel?',
    'Author the sibling story and compare: the beneficiary set shifts toward officers and public-safety interests, epsilon falls toward the coordination floor, and the claimed type migrates toward a transitional-support arrangement with declared sunset conditions.',
    'Adoption would recast the standing arrangement as temporary support rather than unauthorized fabrication, moving the plaintiff seat from full target toward insured-cost bearer and redirecting reform energy from abolition toward sunset design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_protective_scaffold, conceptual, 'Structural delta if the protective-scaffold sibling reading were adopted.').

omega_variable(
    sibling_delta_accountability_void,
    'What would the accountability_void_reading change structurally if adopted as the operative reading of the kernel?',
    'Author the sibling story and compare: the victim set widens to the policed population as a class, epsilon rises above this reading''s value, and the claimed type migrates toward pure extraction with coalition-power analysis for powerless victims.',
    'Adoption would relocate the defect from provenance to operation, making remedy magnitude rather than authorization the reform target and changing which evidence (dismissal statistics versus founding-text analysis) carries the argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_accountability_void, conceptual, 'Structural delta if the accountability-void sibling reading were adopted.').

omega_variable(
    judiciary_gain_concentration,
    'Is the judiciary''s gain from the doctrine a concentrated institutional benefit (agenda power over the statute''s effective scope) or an incidental byproduct of good-faith doctrinal housekeeping?',
    'Certiorari-behavior analysis (does the Court defend the doctrine when squarely challenged and decline reform vehicles?), docket-allocation data, and comparison of the Court''s maintenance effort here against other judge-made defenses it has let lapse.',
    'If the gain is incidental, the judiciary drops out of the concentrated-beneficiary set and the arrangement''s persistence needs a different explanation; if concentrated, capture by the agenda-setting seat is established and the receipt surface firms around the Court.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_gain_concentration, empirical, 'Whether the judiciary''s benefit from the doctrine is concentrated capture or incidental byproduct.').

omega_variable(
    officer_net_position_ambiguity,
    'Do rank-and-file officers experience net benefit or net harm from being governed by an unauthorized, retrospectively defined standard?',
    'Officer surveys on training and certainty costs, liability-insurer pricing data, and outcome comparison under statutory immunity regimes adopted by states after 2020.',
    'If net harm, officers migrate from the beneficiary column toward partial targets, thinning the arrangement''s coordination residue and pushing the computed per-seat classifications toward pure extraction for everyone below the agenda-setting seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_net_position_ambiguity, empirical, 'Whether officers are net beneficiaries or partial targets of the unauthorized standard.').

omega_variable(
    abrogation_cost_class,
    'Is eliminating the doctrine genuinely a one-statute or one-opinion fix, or do entrenchment mechanisms (stare decisis, Senate procedure, organized opposition) make it prohibitive for every seated fixer?',
    'Legislative history of the Ending Qualified Immunity Act and successor bills, state workaround adoption rates, and Supreme Court voting patterns on qualified-immunity certiorari grants.',
    'If the fix is structurally prohibitive, persistence is architectural and the cost-class authoring revises upward; if it is cheap but unused, persistence is preferential — which strengthens this reading''s core claim that nothing about the doctrine is load-bearing beyond judicial will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_cost_class, empirical, 'Whether the doctrine''s removal cost is genuinely low or entrenchment makes it prohibitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_cf_reading_tr_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(qi_cf_reading_tr_t1978, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(qi_cf_reading_tr_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1982, 0.3).
narrative_ontology:measurement(qi_cf_reading_tr_t1991, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1991, 0.33).
narrative_ontology:measurement(qi_cf_reading_tr_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2001, 0.4).
narrative_ontology:measurement(qi_cf_reading_tr_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2009, 0.44).
narrative_ontology:measurement(qi_cf_reading_tr_t2017, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2017, 0.48).
narrative_ontology:measurement(qi_cf_reading_tr_t2025, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2025, 0.52).

% Extraction over time
narrative_ontology:measurement(qi_cf_reading_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(qi_cf_reading_be_t1978, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(qi_cf_reading_be_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(qi_cf_reading_be_t1991, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(qi_cf_reading_be_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2001, 0.66).
narrative_ontology:measurement(qi_cf_reading_be_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2009, 0.7).
narrative_ontology:measurement(qi_cf_reading_be_t2017, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2017, 0.72).
narrative_ontology:measurement(qi_cf_reading_be_t2025, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2025, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(qi_cf_reading_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(qi_cf_reading_su_t1978, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement(qi_cf_reading_su_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(qi_cf_reading_su_t1991, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(qi_cf_reading_su_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(qi_cf_reading_su_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2009, 0.62).
narrative_ontology:measurement(qi_cf_reading_su_t2017, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2017, 0.64).
narrative_ontology:measurement(qi_cf_reading_su_t2025, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, monell_municipal_liability).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'qualified immunity' decomposes into three structurally distinct constraints — one per reading of the qualified_immunity_doctrine kernel. This story (constitutional_fidelity_reading) authors epsilon over the standing arrangement assessed by authorization standards; protective_scaffold_reading authors epsilon over the same arrangement assessed by functional-necessity standards; accountability_void_reading authors epsilon over the same arrangement assessed by extraction magnitude. The upstream member is this reading in the sense that its provenance analysis (no textual or historical foundation) is the premise the accountability_void reading builds upon when arguing the doctrine's operation is indefensible; the scaffold reading cites the same operation as its justification. All three files link one another via affects_constraints; contamination propagates across the family when any member's purity degrades (e.g., a credible historical foundation discovery would simultaneously destabilize this reading and the accountability_void reading while strengthening the scaffold reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, organized, 0.18).
constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
