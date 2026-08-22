% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Market-Licensing Reading (Fourth-Factor Dispositiveness)
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the fair use kernel: the
 *   market-licensing reading, under which the existence of any licensing
 *   mechanism for a use establishes market harm, market harm is dispositive
 *   against the exemption, and fair use survives only where no market exists.
 *   The epsilon referent is the standing arrangement under contest - the
 *   operative interpretive rule that converts licensable uses into infringing
 *   ones - assessed by this reading's own lights, never the transformative
 *   arrangement its rivals would install. The claim/metric independence rule
 *   is honored deliberately: the reading is CLAIMED as tangled_rope (its
 *   proponents sincerely advance a coordination story - administrable
 *   rule-making plus incentive finance - and the structure does retain a
 *   genuine, if thin, coordination function), while the authored metrics
 *   describe heavily extractive, actively enforced operation whose intensity
 *   has grown across five decades. Where the engine's per-seat computations
 *   diverge from that claim, the divergence is the datum.
 *
 * KEY AGENTS:
 *   - - rights_holder_industries: Primary beneficiary with agenda-setting reach (institutional/arbitrage) - converts former exemptions into licensed revenue and shapes the rule through test-case litigation
 *   - - federal_judiciary: Agenda setter (institutional/constrained) - administers the interpretive rule; bound by precedent, cannot exit the role
 *   - - collective_licensing_bodies: Secondary beneficiary (organized/arbitrage) - fee base scales with the licensable universe
 *   - - secondary_creators: Primary target (moderate/identity_locked) - craft practice fused with reuse; every incorporation routes through clearance or risk
 *   - - educational_institutions_and_libraries: Target (organized/constrained) - mission-bound exposure, partial open-access mitigation
 *   - - technology_platforms: Target with superior exit (powerful/arbitrage) - absorbs or monetizes compliance; conversion of compliance into barrier-to-entry
 *   - - the_reading_public: Declared beneficiary bearing indirect costs (powerless/mobile) - near-symmetric position, overridden accordingly
 *   - - unrepresented_takedown_targets: Excluded seat (powerless/trapped) - experiences the rule as automated fee collection; absent from the precedent record
 *   - - ip_law_scholarship: Analytical observer (analytical/analytical) - documents the rationale-operation gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.82).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.78).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Market-Licensing Reading (Fourth-Factor Dispositiveness)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '08c8a969-8069-4919-97a3-8a3f38607659').
narrative_ontology:cs_kernel_codification('08c8a969-8069-4919-97a3-8a3f38607659', fixed_text).
narrative_ontology:cs_authority_grounding('08c8a969-8069-4919-97a3-8a3f38607659', lineage).
narrative_ontology:cs_interpretation_layer_present('08c8a969-8069-4919-97a3-8a3f38607659').
narrative_ontology:cs_reading_relation('08c8a969-8069-4919-97a3-8a3f38607659', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('08c8a969-8069-4919-97a3-8a3f38607659', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('08c8a969-8069-4919-97a3-8a3f38607659', foundational, licensable_use_presumptive_market_harm).
narrative_ontology:cs_axiom_status(licensable_use_presumptive_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('08c8a969-8069-4919-97a3-8a3f38607659', licensable_use_presumptive_market_harm, empirically_contingent).
narrative_ontology:cs_axiom('08c8a969-8069-4919-97a3-8a3f38607659', foundational, market_absence_necessary_for_fair_use).
narrative_ontology:cs_axiom_status(market_absence_necessary_for_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('08c8a969-8069-4919-97a3-8a3f38607659', market_absence_necessary_for_fair_use, instrumental).
narrative_ontology:cs_reference_frame('08c8a969-8069-4919-97a3-8a3f38607659', market_dispositive_property_framework).
narrative_ontology:cs_drift_state('08c8a969-8069-4919-97a3-8a3f38607659', contemporary_ai_licensing_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('08c8a969-8069-4919-97a3-8a3f38607659', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rights_holder_industries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, collective_licensing_bodies).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, the_reading_public).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, secondary_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions_and_libraries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, technology_platforms).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, unrepresented_takedown_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, the_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishers, studios, record labels, and image agencies hold large catalogs and convert previously uncompensated uses into licensed revenue: each precedent treating a licensable use as infringing enlarges the permission-fee base. They shape the rule's application through strategic test-case litigation, model license drafting, and legislative advocacy. Exit is easy in the relevant sense: they can restructure portfolios, move works between exclusive and non-exclusive windows, and shop enforcement across jurisdictions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rights_holder_industries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, rights_holder_industries, agenda_setter).

% Interprets the statutory fair use section and decides, case by case, whether the existence of a licensing market defeats an otherwise arguable exemption. The precedent chain from the 1985 news-quotation ruling through the 1994 archival-copying and parody rulings to the 2015 book-search rulings constitutes the operative rule. Individual judges and panels cannot exit the interpretive role: they are bound by statute, precedent, and appellate supervision, and their discretion is exercised inside a doctrine they did not choose.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Reproduction-rights organizations, performing-rights organizations, and image agencies operate blanket and transactional licensing. Their fee base scales with the breadth of the licensable universe: every use reclassified from exempt to licensable is new billable volume. Their administrative infrastructure lets them redirect effort to adjacent rights-administration markets if demand shifts, so their position is portable even though their revenue depends on the rule.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, collective_licensing_bodies, beneficiary,
    organized, generational, arbitrage, global).

% Documentary filmmakers, biographers, critics, essayists, and sampling musicians whose working method consists of quoting, excerpting, and incorporating existing works. Under this reading each incorporation routes through clearance or carries infringement risk priced into production budgets and insurance. Leaving the arrangement would mean leaving the genres themselves: a critic who cannot quote, a documentarian who cannot show archival footage. Professional identity and craft practice are fused with reuse, so exit is not realistically available even where it is legally conceivable.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, secondary_creators, payer,
    moderate, biographical, identity_locked, global).

% Universities, schools, and libraries run course packets, electronic reserves, digitization projects, and interlibrary lending, all of which touch licensable works. They face periodic licensing demands and audits and have institutionalized permissions offices to manage exposure. They cannot stop teaching or preserving; partial mitigation exists through open-access and public-domain shifts, but the core mission requires using in-copyright material, so their alternatives are narrowed rather than closed.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions_and_libraries, payer,
    organized, generational, constrained, national).

% Search engines, user-generated-content hosts, and AI developers index, host, and train on vast quantities of licensable works and face licensing demands scaled to their revenues. Unlike individual creators they can negotiate blanket licenses, assemble licensed catalogs, relocate infrastructure, or absorb fees as operating cost. Their superior outside options mean the same nominal rule lands on them more softly, and several have converted compliance into a moat that smaller entrants cannot afford.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, technology_platforms, payer,
    powerful, generational, arbitrage, global).

% Audiences receive the arrangement's promised good: a continuing supply of professionally produced content justified by licensing revenue. They also bear its indirect costs: fees passed through in prices, thinner quotation and remix culture, and works accessible only behind permissions. Individually they can substitute what they consume but have no seat in shaping the rule; their stake is diffuse and their voice reaches the doctrine only filtered through the litigating parties.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, the_reading_public, beneficiary,
    powerless, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, the_reading_public, payer).

% Individual uploaders, small bloggers, fan creators, and teachers who receive automated takedown notices or settlement demands for uses the case law might well protect. Almost none litigate: the cost dwarfs the stakes, so they comply, remove, or pay. Because they never appear as parties, the body of precedent defining the reading is written entirely by repeat players on both sides, and the operating reality of the rule for ordinary users is invisible to it.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, unrepresented_takedown_targets, excluded,
    powerless, immediate, trapped, global).

% Academic commentators track the doctrine's movement across circuits and decades, documenting the widening gap between the stated incentive rationale and the observable pattern of licensing expansion onto already-created works. They hold no stake in particular outcomes and their analyses feed both the bench and the benefiting industries without belonging to either.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, ip_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, rights_holder_industries).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies an administrable decision rule where the statutory text is open-ended: creators, rights holders, and courts get a single tractable question (does a licensing market exist or plausibly could?) in place of open-textured transformation analysis, and reuse demand is channeled into organized licensing markets that finance ongoing production and rights administration.
% TRANSFER_FUNCTION: Moves money (license fees, settlement payments, clearance budgets) from everyone whose work product touches a licensable work - secondary creators, educators, libraries, platforms - to rights holders and collective licensing bodies; and moves expressive latitude (unpaid quotation, excerpt, remix, archival display) from the user public into paid permission.
% ABSENT_VOICES: Unrepresented takedown targets and the diffuse user public are absent: the reading is elaborated in litigation between repeat players (major publishers and labels versus major platforms and institutions), while the individuals who actually experience the rule as automated fee collection settle or comply and never enter the record. Non-market values - cultural participation, memory-institution missions, cumulative creativity - have no litigating seat at all.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, clearance departments, permissions budgets, blanket-license fee structures, and takedown-settlement pipelines would lose their doctrinal foundation; secondary creators would resume uncompensated incorporation as of right; licensing bodies would see their transaction volumes contract to genuinely negotiated markets; and pricing and insurance structures built around infringement exposure would reprice. The reuse economy is organized around this rule and would visibly reorganize without it.
% FOUNDING_PROBLEM: Two problems at codification: courts needed a tractable metric for an open-ended statutory exemption, and rights holders needed protection against copying technologies (photocopying, then digital distribution) that threatened specific existing markets. The market factor supplied both: administrability for the bench, and a property-consistent boundary for incumbents.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the 1985 Supreme Court opinion itself grounds the factor in administrability and protection of the specific market for the work, and the legislative history of the 1976 Act frames it as a guide for courts facing new technologies. Legal scholarship across the spectrum attests that these founding problems were real. What NO corroborating source outside the benefiting parties attests is the reading's universal quantifier - that ANY licensable use harms the market for licensed uses; that extrapolation is the reading's own contribution, and the critical scholarship (and the 1994 and 2015 pushback opinions) expressly rejects it.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82 at interval end) because the reading's universal quantifier places every quotable, excerptable, trainable, archivable use inside the permission economy, and the fee levels are set by bargaining position rather than marginal cost. Suppression (0.78) is authored as a raw structural property - unscaled by power or scope, per the framework rule - reflecting dependence on active exclusion of the unlicensed alternative: takedown regimes, settlement leverage, and clearance culture, not participant preference, hold the arrangement in place. Theater ratio (0.34) is moderate-low: the adjudicative function still operates (courts do decide real cases), but a growing share of activity is performative compliance - permissions sought for uses with strong exemption claims, insurance and budget line-items that exist to demonstrate diligence. Accessibility collapse (0.52) is mid-range: open licensing, public-domain growth, and the transformative-use rival keep alternatives partly available, but each successful licensing market forecloses the corresponding unlicensed practice. Resistance (0.72) is high and documented: the 1994 parody opinion expressly demoted market harm from 'single most important element,' and the 2015 book-search ruling found exempt a use with an obvious licensing market, on transformation grounds. The measurement series share ONE grid (1976, 1985, 1994, 2003, 2012, 2021, 2026) across all three tracked metrics. The extractiveness series is not monotonic: it peaks locally in 1994 (archival-copying ruling), dips through 2003-2012 as the transformation counter-current wins marquee cases, then resumes climbing as streaming consolidation and AI training-data licensing extend the licensable universe faster than doctrine contracts it. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the interval opens in a world of sparse, expensive litigation and closes in one of automated detection, notice-and-takedown at scale, and industrialized clearance - a genuine ratchet in the machinery needed to hold the arrangement, not merely a shift in extraction. The 2026 endpoints are marked projected: the AI-licensing wave is mid-formation at generation time.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the rights-holder and licensing-body positions the arrangement is property enforcement they built and staffed: the same structure that reads as extraction from the documentary filmmaker's chair reads as the boundary that makes catalog investment rational from theirs. The judiciary's seat is neither: it administers a rule whose distributive consequences it did not choose, constrained by precedent in both directions. Among targets, exit quality drives divergence: the platform seat's arbitrage-grade options damp its effective burden (and let it convert compliance into a moat against smaller rivals), while the secondary-creator seat's identity lock amplifies theirs - the filmmaker cannot stop being a filmmaker, so the fee is unavoidable in a way the platform's is not. The excluded seat experiences a rule harsher than the case-law record shows, because the record is written only by parties wealthy enough to litigate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (rights_holder_industries, collective_licensing_bodies) drive those seats toward the subsidy end; victim declarations (secondary_creators, educational_institutions_and_libraries, technology_platforms, unrepresented_takedown_targets) drive them toward the target end, with the engine's exit modulation doing real work: identity_locked secondary creators sit nearer full-target than arbitrage-grade platforms despite both being declared payers. One override is declared: the_reading_public derives a strongly beneficiary-side directionality from its beneficiary listing, but its true structural relationship is near-symmetric - it receives the incentive-system's promised output while carrying price pass-through and a thinned quotation-and-remix commons - so the override sets the powerless seat to d=0.45. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems - adjudicative administrability and protection of specific existing markets - remain live, so the arrangement is not mandatrophy-resolved and no sunset claim is authored. The mandatrophy-relevant risk sits elsewhere: the self-referential market-creation omega. If licensing markets are largely artifacts of the rule itself, the doctrine's adjudicative function atrophies into clearance ritual - courts ratify a permission economy that no longer needs adjudicating - and theater_ratio becomes the leading indicator. The authored series shows theater climbing slowly (0.12 to 0.34) without yet dominating; the piton-drift watch condition is theater exceeding roughly half of activity combined with a dead founding problem, which the current data do not meet. The classification prevents mislabeling in both directions: reading the structure as pure extraction (snare) would erase the genuine administrability function the bench still performs and the sincere incentive rationale a substantial fraction of its adherents hold; reading it as pure coordination (rope) would erase the asymmetric transfer that is its dominant observable output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (market_licensing_reading) of the kernel fair_use_statutory_exception (17 U.S.C. Sec. 107 as a stabilized text). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the sibling stories: transformative_right_reading and narrow_defense_reading instantiate the same statutory text with different fourth-factor weights. The disagreement is located in (a) whether market harm is DISPOSITIVE or merely one weighted factor, and (b) whether absence of a licensing market is a NECESSARY condition of fair use or merely sufficient. This story fixes (a) dispositive and (b) necessary.',
    'Under transformative_right_reading, epsilon drops substantially for transformative uses (the licensable-but-transformative class leaves the target set) and the computed type moves toward rope/scaffold; under narrow_defense_reading, epsilon stays high but the target set is bounded by enumerated purposes, moderating scope-driven amplification. Classification of THIS story is valid only for the market-licensing instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: kernel membership, sibling readings, and the located axis of disagreement.').

omega_variable(
    self_referential_market_creation,
    'Does a licensing market exist independently of this reading, or does the reading create the licensing market it then cites as proof of harm?',
    'Natural experiments comparing post-adjudication trajectories: where courts found fair use despite licensability (search indexing, certain archival digitization), did licensing markets nonetheless emerge and thrive? Where courts found infringement (music sampling), did the resulting licensing market reflect pre-existing demand or demand manufactured by liability exposure?',
    'If licensing markets are largely reading-created, the causal premise ''any licensable use harms the market for licensed uses'' is circular: the harm is an artifact of the rule, and the genuinely pre-existing component of epsilon is far smaller than measured. The reading''s justification would collapse from empirical claim to self-fulfilling institution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_referential_market_creation, empirical, 'Whether the market-harm premise is empirically grounded or self-referential.').

omega_variable(
    enforcement_visibility_bias,
    'Do the authored metrics understate the reading''s operative force because most of its enforcement occurs below the visible litigation record?',
    'Compare published-case outcomes against the volume of takedown notices, settlement demands, and clearance transactions: estimate the ratio of uncontested enforcement events to litigated ones, and survey clearance-culture behavior (permissions sought for uses with strong fair use claims).',
    'If the sub-litigation layer dominates, effective suppression and extractiveness are higher than case-law-visible values suggest; the doctrine''s practical bite exceeds its doctrinal footprint, and the 2012 dip in the extractiveness series may be a litigation-record artifact rather than a real relaxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_visibility_bias, empirical, 'Visibility bias: case law samples only contested instances of the reading''s operation.').

omega_variable(
    reading_scope_spectrum,
    'Is the epsilon referent the reading as an operative judicial rule (bounded by Campbell-lineage pushback) or the reading as its maximalist principle (any licensable use barred, fair use null in practice)?',
    'Per the epsilon-invariance rule this story fixes ONE referent: the operative interpretive rule including its aspirational pull on clearance practice and licensing-product design. Resolution would come from decomposing the spectrum into separate stories if the bounded-application and maximal-principle versions show stably divergent epsilon.',
    'The maximalist-principle version would carry higher epsilon and likely compute as snare; the strictly-bounded-application version would carry lower epsilon and stay tangled_rope. The authored 0.82 sits at the operative-rule end, pulled upward by the principle''s effect on behavior that never reaches a courtroom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_spectrum, conceptual, 'Referent selection within the reading''s own spectrum of application.').

omega_variable(
    trips_three_step_exportability,
    'Can a market-dispositive fair use reading survive the Berne/TRIPS three-step test (limitations must be confined to certain special cases, not conflict with normal exploitation, not unreasonably prejudice legitimate interests) when exported to other jurisdictions?',
    'Track WTO dispute consultation records and national implementation of fair-use-style exceptions: does any trading partner accept a reading under which routine quotation, education, and platform uses fail the ''normal exploitation'' step?',
    'If the maximal reading fails step-two scrutiny internationally, its scope contracts to the US domestic frame, reducing spatial-scope amplification of effective extraction; if partners adopt it, the reading''s extraction scales globally and hardens against domestic reform via trade-pressure feedback.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trips_three_step_exportability, conceptual, 'International durability of the reading under treaty step-test constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fumr_tr_t1976, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement(fumr_tr_t1985, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(fumr_tr_t1994, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement(fumr_tr_t2003, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement(fumr_tr_t2012, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(fumr_tr_t2021, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(fumr_tr_t2026, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2026, 0.34).

% Extraction over time
narrative_ontology:measurement(fumr_be_t1976, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement(fumr_be_t1985, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(fumr_be_t1994, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1994, 0.72).
narrative_ontology:measurement(fumr_be_t2003, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2003, 0.68).
narrative_ontology:measurement(fumr_be_t2012, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement(fumr_be_t2021, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement(fumr_be_t2026, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2026, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(fumr_su_t1976, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1976, 0.35).
narrative_ontology:measurement(fumr_su_t1985, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1985, 0.44).
narrative_ontology:measurement(fumr_su_t1994, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1994, 0.52).
narrative_ontology:measurement(fumr_su_t2003, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(fumr_su_t2012, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2012, 0.62).
narrative_ontology:measurement(fumr_su_t2021, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement(fumr_su_t2026, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, dmca_notice_and_takedown).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'fair use' covers one statutory kernel (17 U.S.C. Sec. 107) instantiated as three structurally distinct constraints. This story (market_licensing_reading) carries the highest epsilon of the family: it renders market harm dispositive and market absence necessary. narrow_defense_reading is the conservative upstream baseline (property framing, defense narrowly construed) from which this reading extends; transformative_right_reading is the downstream rival whose operating environment this reading degrades - every licensing market this reading successfully establishes shrinks the space in which transformative claims can be pressed, which is why the edge runs from this story to the sibling. Epsilon differs across the family because the target set differs: all-licensable-uses here, enumerated-purpose uses there, transformative uses in the third. Each member is authored as a separate file with its own stable epsilon per the decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
