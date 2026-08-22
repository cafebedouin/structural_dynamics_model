% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_narrow_defense, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Copyright-as-Property Reading)
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   Fair use is a statutory exception to copyright infringement, enacted as
 *   an affirmative defense. This constraint models ONE READING of fair use:
 *   the narrow-defense reading, which treats copyright primarily as property
 *   and fair use as a narrow exception that must not erode the copyright
 *   holder's market value. Under this reading, commercial uses are
 *   disfavored, transformative value is underweighted in the four-factor
 *   test, and the burden falls on the defendant to prove no market harm. The
 *   constraint's operation is substantially extractive: most unauthorized
 *   uses require licensing or face infringement liability; reusers without
 *   market access or legal resources are suppressed; and the doctrine
 *   preserves the copyright holder's control over derivative and secondary
 *   markets. This reading coexists with two sibling readings
 *   (transformative-right reading, market-licensing reading) that instantiate
 *   structurally different constraints with different ε values and victim
 *   structures. The claim and metrics are intentionally independent: claimed
 *   as tangled_rope (coordination + extraction), authored as highly
 *   extractive (0.81) and increasingly so over time.
 *
 * KEY AGENTS:
 *   - copyright_holders: agenda-setting beneficiaries (institutional power); shape doctrine, control licensing, benefit from narrow fair use
 *   - educational_institutions: dual-positioned payers and minor beneficiaries (organized power); constrained exit; face escalating licensing demands
 *   - reusers_without_market_access: powerless victims (identity_locked exit); suppressedly excluded from reuse unless they can bear litigation costs
 *   - transformative_creators: moderate-power payers (constrained exit); face doctrine that treats success as evidence of unfairness
 *   - courts_applying_fair_use: institutional observers (analytical power); apply the four-factor test under this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.81).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.68).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Copyright-as-Property Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'da0b380d-33fe-4df7-aa4a-bd557eba0bc9').
narrative_ontology:cs_kernel_codification('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', fixed_text).
narrative_ontology:cs_authority_grounding('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', extraction).
narrative_ontology:cs_interpretation_layer_present('da0b380d-33fe-4df7-aa4a-bd557eba0bc9').
narrative_ontology:cs_reading_relation('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', fair_use_statutory_exception__transformative_right_reading, influences).
narrative_ontology:cs_reading_relation('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', foundational, copyright_as_property_analogue).
narrative_ontology:cs_axiom_status(copyright_as_property_analogue, holdable).
narrative_ontology:cs_axiom_grounding('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', copyright_as_property_analogue, conventional).
narrative_ontology:cs_axiom('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', foundational, market_value_preservation_primacy).
narrative_ontology:cs_axiom_status(market_value_preservation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', market_value_preservation_primacy, instrumental).
narrative_ontology:cs_axiom('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', secondary, commercial_use_presumptive_unfairness).
narrative_ontology:cs_axiom_status(commercial_use_presumptive_unfairness, holdable).
narrative_ontology:cs_axiom_grounding('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', commercial_use_presumptive_unfairness, empirically_contingent).
narrative_ontology:cs_reference_frame('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', copyright_as_exclusive_property_right).
narrative_ontology:cs_drift_state('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', contemporary_digital_abundance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da0b380d-33fe-4df7-aa4a-bd557eba0bc9', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, professional_licensing_markets).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, reusers_without_market_access).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, transformative_creators_under_market_threat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, library_preservation_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, commercial_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, library_preservation_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own exclusive rights to creative works and control licensing decisions. Shape fair use doctrine through litigation strategy, expert testimony, and statutory advocacy. Benefit directly from narrow fair use because it expands licensing demand. Can exit the constraint entirely (they own the copyrights); their directionality (d ≈ 0.08) reflects full beneficiary status.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, beneficiary).

% Use copyrighted materials extensively for teaching, research, and library functions. Face escalating licensing costs and copyright claims even for uses (like course-pack copying) that were once tolerated. Benefit from access to published works but are constrained by licensing agreements and fear of infringement liability. Cannot exit without abandoning educational mission.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, beneficiary).

% Include academic researchers, student creators, remix artists, and low-income reusers who want to build on copyrighted material but cannot afford licensing. Suppressed by legal uncertainty, litigation risk, and inability to mount a fair use defense. Identity as creators, scholars, or cultural producers is fused with the ability to engage with existing copyrighted works; exit means abandoning the field. Directionality override (d ≈ 0.92) reflects near-total target status.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, reusers_without_market_access, payer,
    powerless, biographical, identity_locked, universal).

% Make parodies, critiques, transformative remixes, and derivative art that build on copyrighted originals. Under the narrow reading, their work is fair use only if it does not compete with any market the original copyright holder might develop. If the derivative work is successful, that success is used as evidence of market substitution and unfairness. Face a paradox: prove the work is transformative (valuable) without proving it competes (harmful).
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_creators_under_market_threat, payer,
    moderate, biographical, constrained, global).

% Include search engines, data aggregators, AI training companies, and software firms that build services or products using copyrighted content. Under the narrow reading, commercial nature is often determinative against fair use, regardless of transformativeness or public benefit. Many can afford licensing and are channeled into licensing markets. Those that cannot (e.g., training data for AI models not explicitly licensed) face high litigation risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, commercial_reusers, payer,
    powerful, biographical, mobile, global).

% Apply the statutory four-factor test to infringement disputes. Under the narrow reading, they weight commercial use heavily, treat market harm broadly (including hypothetical licensing markets), and often presume copyright holders' framing of market value without requiring empirical evidence. Sit between copyright holders (who frame cases to support narrow fair use) and reusers (who argue for broader exceptions).
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts_applying_fair_use, observer,
    institutional, generational, analytical, national).

% Argue for shorter copyright terms, broader fair use, and a robust public domain where works can be freely reused. Excluded from the doctrine's framing because they contest the underlying premise (that market value preservation should be the primary goal). Must persuade legislators to change the statute rather than courts to reinterpret it under current doctrine.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates, excluded,
    moderate, generational, constrained, global).

% Preserve digital and physical collections for posterity, including copyrighted works. Face legal uncertainty about preservation copying, format-shifting, and archival access. Benefit from copyright-protected works in their collections but are suppressed by the narrow reading's restriction on preservation uses not explicitly licensed. Cannot exit without abandoning their preservation mission.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, library_preservation_institutions, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, library_preservation_institutions, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a licensing marketplace where copyright holders can control and monetize reuse of their creative works. Users negotiate terms or seek permission; publishers invest in creation and distribution knowing they own exclusive rights. The system coordinates who can use what, at what cost, and under what conditions, solving the collective-action problem of how to fund creative work while permitting some derivative uses.
% TRANSFER_FUNCTION: Transfers the right to copy, distribute, display, or perform copyrighted material from copyright holders to licensees, typically for a fee. Under the narrow fair use reading, the copyright holder's permission is required for most uses except those with zero market harm to their licensing demand. The constraint channels reusers into licensing markets or into suppression (non-use).
% ABSENT_VOICES: Transformative reusers, educational institutions, researchers, library systems, and creative communities that depend on access to existing works without per-use licensing are structurally excluded from the doctrine's framing. They must prove their case defensively (that their use is fair) rather than affirmatively (that their use should be permitted). Public-domain advocates who argue copyright terms are too long and fair use too narrow are excluded from the statutory interpretation; they must change the law through legislation, not through doctrine.
% DISAPPEARANCE_RATIONALE: If this narrow fair use doctrine were replaced by a broader or transformative-centered doctrine, the licensing market would contract sharply. Uses currently requiring licenses (or suppressed due to legal uncertainty) would become permissible without fees. Copyright holders would lose licensing revenue or be forced to develop new business models. Reusers, educators, and transformative creators could build derivative works without permission. The allocation of control over secondary markets and cultural production would shift dramatically toward reusers.
% FOUNDING_PROBLEM: Early copyright doctrine protected copyright holders from rampant unauthorized copying but risked blocking legitimate reuse, commentary, and scholarship. Fair use was enacted as a safety valve to allow uses that served public purposes (criticism, education, parody) without requiring permission or payment.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders argue the founding problem persists: digital copying is effortless and copyright enforcement must remain strict to preserve incentives for creation. Opposing scholars, librarians, technologists, and some judges argue the founding problem has been solved and inverted: copyright is now so strictly enforced that fair use no longer functions as a meaningful check on copyright monopoly. Legislative and academic testimony from outside the copyright-holder set supports the inverted-problem reading. Economic studies show that current fair use doctrine rarely permits reuse even when social benefit exceeds private cost to copyright holders.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.62 to 0.81 over the interval (t0 to t40) as the doctrine's application hardened, licensing markets expanded, and courts increasingly treated commercial use and market substitution as determinative. The theater ratio stays low (0.12 to 0.29) because the constraint's core function—routing reuses into licensing markets—is genuinely operational, not theatrical; the performative element is the court's framing of narrow fair use as a balance when it increasingly favors copyright holders. Suppression is moderate-high (0.54 to 0.68) and rising because reusers face three suppressive forces: (1) legal uncertainty—fair use claims are expensive to litigate and outcomes are unpredictable under the narrow reading; (2) technical suppression—DRM and contractual lock-in prevent reuse even where fair use might apply; (3) market suppression—reusers without market access cannot afford licensing and cannot mount a credible fair use defense because commercial nature is presumptively unfair. Accessibility collapse is high (0.72): alternatives (seeking permission, waiting for public domain, building on non-copyrighted materials) are real but limited; the constraint effectively narrows the design space for derivative works.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (copyright holders) sees narrow fair use as legitimate copyright protection; the constraint enforces their property right and preserves the licensing market. Powerless reusers see it as a tool of exclusion and a brake on cultural production. Courts sit in the middle but tend to adopt the agenda-setter's framing because copyright holders shape litigation, control precedent-setting cases, and frame the doctrine as property protection. The engine should compute markedly different types from the agenda-setter's seat (rope-like: coordination benefit) versus the powerless reuser's seat (snare-like: pure extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders (institutional power, arbitrage exit): d near 0.0 (full beneficiary). They control the rule-making, collect licensing fees, and can exit the constraint at any time (they own the copyrights). Educational institutions (organized power, constrained exit): d near 0.45 (nearly symmetric). They benefit from copyright-protected works but face escalating licensing demands and reduced teaching flexibility. Powerless reusers (powerless, identity_locked exit): d near 0.95 (near-full target). They cannot afford licensing, cannot mount a legal defense, and cannot avoid the constraint by exiting the field (their identity as creators/educators/researchers depends on engaging with copyrighted material). Transformative creators (moderate power, constrained exit): d near 0.75 (high target). They bear the cost of uncertain doctrine and the risk that their success will be used against them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was copyright-holder monopoly suppressing legitimate reuse. The narrow fair use doctrine was meant to preserve space for commentary, scholarship, and cultural response. However, the doctrine has inverted: it now protects copyright holders' monopoly by construing fair use so narrowly that most reuse requires licensing. The founding problem (too-broad copyright) has been replaced by a new problem (fair use too narrow to function as a meaningful check on copyright monopoly). This is classic mandatrophy: the remedy has become the disease. The constraint persists because copyright holders have captured the doctrine's evolution and courts apply it accordingly. The high suppression (0.68) and rising extractiveness (0.62→0.81) are symptoms of mandatrophy: the doctrine no longer balances but instead preserves market value, which was the original problem it was meant to address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_epistemic_burden,
    'Who bears the burden of proving market harm or its absence? Under the narrow reading, defendants must prove no market harm; under alternative readings, plaintiffs must prove actual harm.',
    'Compare litigation outcomes and settlement patterns in jurisdictions that reverse the burden: if reuse increases and licensing markets contract, the burden itself was suppressive; if reuse stays constant, the burden was evidentiary only.',
    'Reversing the burden would shift d for reusers downward (less suppressed) and shift the constraint from tangled_rope toward rope or away from snare. Classification would change per seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_epistemic_burden, empirical, 'Whether the burden of proof itself is a suppressive mechanism or merely a legal formalism.').

omega_variable(
    hypothetical_licensing_market_boundary,
    'How broad is the ''licensing market'' the doctrine must preserve? Does it include markets that might theoretically exist if the copyright holder chose to develop them, or only markets currently active?',
    'Examine statutory amendments or court decisions that define licensing market scope; compare to economic evidence of actual licensing revenue in each sector.',
    'A broad reading (hypothetical markets count) expands the constraint''s extractiveness; a narrow reading (only active markets) shrinks it. The reading shapes whether transformative uses qualify as fair use.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypothetical_licensing_market_boundary, conceptual, 'Whether market preservation includes hypothetical or only actual licensing markets.').

omega_variable(
    transformative_vs_substitution_ambiguity,
    'Can a use be both transformative AND substitutional? If a parody is successful, does success prove market substitution (unfair) or cultural value (fair)?',
    'Track court decisions on transformative uses that compete with original works (parodies, song covers, fan works). If courts increasingly treat success as evidence of unfairness, the doctrine has shifted toward pure extraction.',
    'If transformativeness and commercial success are treated as contradictory, reusers face a paradox: prove your work adds value AND prove it doesn''t compete. Resolving this would shift d for transformative creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_vs_substitution_ambiguity, conceptual, 'Whether success of a transformative work counts for or against fair use.').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the measured suppression (0.68) structural (licensing costs, legal risk) or partially internalized (reusers accept the doctrine as legitimate and self-censor)?',
    'Post-doctrinal-shift analysis: if fair use doctrine were significantly broadened, would reuse surge (indicating internalized suppression is reversible) or stay flat (indicating structural barriers dominate)?',
    'If suppression is internalized, changing the doctrine alone will not free reuse; creators have fused their identity with copyright-owner expectations. If suppression is structural, broadening doctrine would release suppressed reuse quickly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether the constraint''s suppressive force is structural or internalized in reuser identities.').

omega_variable(
    kernel_reading_conflict_transformative_vs_narrow,
    'This reading treats copyright as property and fair use as narrow exception. The transformative reading treats fair use as a positive right to enable cultural production. Can these readings coexist in a single legal framework, or do they foreclose each other?',
    'Examine whether courts can apply both framings in different cases, or whether precedent in one reading undercuts the other. Check whether any single party holds both positions (indicating coexistence) or parties divide cleanly by reading (indicating foreclosure or influence).',
    'If the readings foreclose each other, one must eventually dominate; if they coexist, both remain live options and the constraint-space has structural ambiguity. If one influences the other (as hypothesized), expanding the narrow reading contracts the transformative reading''s domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_conflict_transformative_vs_narrow, conceptual, 'Whether the narrow and transformative readings of fair use can coexist within a single legal framework or whether they are mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fair_tr_t5, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(fair_tr_t15, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(fair_tr_t20, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fair_tr_t25, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(fair_tr_t35, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 40, 0.29).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(fair_be_t5, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(fair_be_t15, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(fair_be_t20, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(fair_be_t25, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(fair_be_t35, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(fair_su_t5, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(fair_su_t15, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(fair_su_t20, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(fair_su_t25, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(fair_su_t35, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 35, 0.68).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_term_extension__extraction_side).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, digital_millennium_copyright_act__circumvention_prohibition).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fair_use_statutory_exception kernel. The narrow-defense reading instantiates high extractiveness (0.81), treating copyright as property and fair use as narrow exception. The transformative-right reading instantiates lower extractiveness (projected 0.35-0.45), treating fair use as enabling cultural production. The market-licensing reading instantiates highest extractiveness (projected 0.88+), requiring licensing for any use that could substitute for a licensed use. All three share the same statutory text (17 U.S.C. § 107) but produce different constraints through interpretive framing. Network edges: (1) narrow-defense reading influences transformative reading by contracting the space where transformative uses qualify; (2) narrow-defense reading influences market-licensing by providing doctrinal leverage for market-preservation arguments; (3) all three affect downstream constraints on term extension and DMCA circumvention (broad copyright doctrine amplifies both).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, powerless, 0.92).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
