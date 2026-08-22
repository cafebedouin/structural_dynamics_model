% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Temporary Public-Domain Scaffold (Public-Scaffold Reading)
 *   domain: intellectual_property/constitutional/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   copyright_constitutional_mandate kernel: the public_scaffold_reading,
 *   under which the constitutional grant of exclusive rights 'for limited
 *   Times' is a strictly transitional instrument whose endpoint is the public
 *   domain. The arrangement the story is ABOUT — the ε referent — is the
 *   standing copyright bargain as operated under this reading's framework: a
 *   time-boxed monopoly that funds creation and then releases works to the
 *   commons, assessed by this reading's own lights (incentive-tethered terms,
 *   meaningful fair use, mandatory release). Per the ε-invariance principle,
 *   the sibling readings are different constraints in different files:
 *   corporate_enclosure_reading (copyright as property demanding maximal
 *   protection) and judicial_ambiguity_reading (term length as unconstrained
 *   legislative discretion) share this referent but author different ε over
 *   it. Assumptions stated plainly: the frame is US constitutional (Article
 *   I, Section 8, Clause 8) with Berne-era treaty overlay; interval 0–55 maps
 *   approximately to 1970–2025 (pre-1976 Act through the post-CTEA/DMCA era).
 *   The claimed_type is scaffold because the reading's charter is
 *   transitional by definition — 'limited Times' IS a sunset clause — while
 *   the authored metrics describe observed operation, which has drifted away
 *   from that charter (serial term extensions, formalities abolition,
 *   anti-circumvention enforcement). The claim/metric gap is deliberate and
 *   is the datum: a scaffold whose sunset keeps being postponed is exactly
 *   the degradation pattern this corpus exists to detect.
 *
 * KEY AGENTS:
 *   - congress_legislature: agenda setter (institutional/arbitrage) — sets term lengths and enforcement scope under the Copyright Clause; bears no direct cost from extensions
 *   - commercial_rights_holders: primary in-term beneficiary (institutional/arbitrage) — collects licensing revenue, treats catalog duration as an asset, funds extension lobbying
 *   - authors_and_creators: dual-positioned beneficiary-payer (organized/constrained) — collects the exclusive window now, contributes to the commons later
 *   - general_reading_public: diffuse beneficiary (moderate/constrained) — free access to expired works, priced access to in-term works
 *   - downstream_creators: beneficiary-payer (moderate/constrained) — builds on the commons, licenses or designs around in-term works
 *   - libraries_and_archives: steward-beneficiary (organized/constrained) — operates under statutory exceptions it cannot self-provide
 *   - public_domain_commons: terminal beneficiary, non-agent commons (powerless/civilizational) — grows only by term expiry, has no representative
 *   - future_generations: excluded absent voice (powerless/trapped) — inherits whatever today's term votes leave unenclosed
 *   - orphan_works_users: excluded seat (powerless/trapped) — frozen out by automatic rights and unlocatable owners
 *   - supreme_court: analytical observer (institutional/analytical) — reviews the mandate's limits and has so far deferred to Congress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.53).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.48).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Temporary Public-Domain Scaffold (Public-Scaffold Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '4582c432-9ded-46b8-9454-e3abbb8d197f').
narrative_ontology:cs_kernel_codification('4582c432-9ded-46b8-9454-e3abbb8d197f', fixed_text).
narrative_ontology:cs_authority_grounding('4582c432-9ded-46b8-9454-e3abbb8d197f', lineage).
narrative_ontology:cs_interpretation_layer_present('4582c432-9ded-46b8-9454-e3abbb8d197f').
narrative_ontology:cs_reading_relation('4582c432-9ded-46b8-9454-e3abbb8d197f', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('4582c432-9ded-46b8-9454-e3abbb8d197f', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('4582c432-9ded-46b8-9454-e3abbb8d197f', foundational, monopoly_temporary_means_not_end).
narrative_ontology:cs_axiom_status(monopoly_temporary_means_not_end, holdable).
narrative_ontology:cs_axiom_grounding('4582c432-9ded-46b8-9454-e3abbb8d197f', monopoly_temporary_means_not_end, instrumental).
narrative_ontology:cs_axiom('4582c432-9ded-46b8-9454-e3abbb8d197f', foundational, public_domain_is_default_state).
narrative_ontology:cs_axiom_status(public_domain_is_default_state, holdable).
narrative_ontology:cs_axiom_grounding('4582c432-9ded-46b8-9454-e3abbb8d197f', public_domain_is_default_state, deontological).
narrative_ontology:cs_axiom('4582c432-9ded-46b8-9454-e3abbb8d197f', secondary, retrospective_extension_zero_incentive).
narrative_ontology:cs_axiom_status(retrospective_extension_zero_incentive, holdable).
narrative_ontology:cs_axiom_grounding('4582c432-9ded-46b8-9454-e3abbb8d197f', retrospective_extension_zero_incentive, instrumental).
narrative_ontology:cs_reference_frame('4582c432-9ded-46b8-9454-e3abbb8d197f', public_domain_enrichment_bargain).
narrative_ontology:cs_drift_state('4582c432-9ded-46b8-9454-e3abbb8d197f', contemporary_post_ctea_dmca_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4582c432-9ded-46b8-9454-e3abbb8d197f', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain_commons).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, general_reading_public).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, commercial_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, idea_expression_dichotomy).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, fair_use_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulated stock of works anyone may freely use, adapt, and build upon. Grows by one route only: works whose terms expire. Has no voice, no budget, and no representative; its size is set entirely by decisions made about other parties' rights. Cannot decline additions or object to delays.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain_commons, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain_commons).

% Writes and rewrites the copyright statute under the constitutional grant to secure exclusive rights for limited times to promote the progress of science. Sets term lengths, exception scopes, and enforcement remedies. Extending terms imposes no direct cost on members and generates concentrated industry support; shortening them mobilizes the same industries against the sponsor. Treaty commitments constrain how far terms can fall without renegotiation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive the exclusive window on new work — the ability to license, sell, or withhold reproduction for a term — and rely on that same window's expiry to reach the accumulated commons of everything written before them. Every creator stands on both sides of the clock: collector now, contributor later. Individual creators rarely litigate or lobby directly; publishers and estates act for them.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators, payer).

% Publishers, studios, labels, and estates that administer large catalogs. Collect licensing revenue throughout each term and treat catalog duration as a balance-sheet asset. Fund the lobbying and litigation that seek longer terms and broader enforcement; can shift investment across jurisdictions and media formats if any single regime turns hostile.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, commercial_rights_holders, beneficiary,
    institutional, biographical, arbitrage, global).

% Reads, watches, and listens under a mix of free access (expired works, library exceptions, fair use) and paid access (in-term works). Bears the price effects of longer terms as delayed entry of works into free availability. Participation is diffuse: no member feels the cost sharply enough to organize around it, though library and archive institutions aggregate some of the interest.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, general_reading_public, beneficiary,
    moderate, generational, constrained, national).

% Make new work out of old — adaptations, scholarship, sampling, restorations, training corpora. Draw freely from expired works; must license, design around, or accept litigation risk for in-term ones. Uncertainty about a work's status, which grew after registration formalities disappeared, can freeze projects outright.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators, payer).

% Preserve collections and lend them to the public under statutory exceptions. Steward the physical carriers of works approaching term expiry and advocate for preservation-friendly rules. Depend on legislative goodwill for the exceptions they operate under; cannot opt out of the term structure governing their collections.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, libraries_and_archives, beneficiary,
    organized, generational, constrained, national).

% Will inherit whatever enters the commons under the terms set today. Each extension enacted now holds back works already created, already paid for, and previously scheduled for release. They cast no vote, hold no rights, and appear in term debates only as abstraction.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Want to republish, restore, or adapt works whose rights-holders cannot be located — out-of-print books, abandoned films, defunct labels' recordings. Since formalities disappeared, rights attach automatically and run decades past any commercial life, so the safe path (locating the owner) often terminates nowhere and the risky path carries statutory damages. They sit outside the bargaining table entirely.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, orphan_works_users, excluded,
    powerless, biographical, trapped, national).

% Reviews whether copyright legislation stays within the constitutional grant. Has upheld term extensions against challenge, reasoning that the clause's limits are Congress's to define in the first instance. Its docket determines whether the mandate's limits are enforceable anywhere outside Congress itself.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, commercial_rights_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public-goods underproduction problem for creative works: creative output is non-rival and costly to exclude, so absent exclusive rights, copyists free-ride and front-loaded creation costs go unrecovered. The arrangement coordinates by granting a time-boxed exclusive exploitation window on a published schedule, so every work converts into common stock at term's end. It also coordinates reuse: term clocks, fair use, and the idea/expression line tell everyone what is free and what is not.
% TRANSFER_FUNCTION: Moves exclusive exploitation rights from the public to authors (and their assignees) for a limited term — during which licensing fees move from users and distributors to rights-holders — then moves the works themselves, irrevocably, into the public domain.
% ABSENT_VOICES: Future generations — whose public-domain inheritance is set by each term-extension vote — have no seat; orphan-work users locked out by formalities abolition have no procedural home; the diffuse reading public is represented only by proxy through library institutions. Apparent unanimity in term extensions arises partly because the constituency harmed by them (future users of today's works) is structurally absent from the room.
% DISAPPEARANCE_RATIONALE: Publishing, music, film, and software economics are built around the exclusive window; overnight removal would force wholesale repricing, new funding models (patronage, subscription, public subvention), and an immediate commons of all in-term works — a massive rearrangement, even granting this reading's own claim that creation would continue under alternative finance.
% FOUNDING_PROBLEM: The Statute of Anne / Copyright Clause problem: break the private perpetual book monopolies that had choked the English book trade while ensuring new writing still got produced — securing exclusive rights 'for limited Times' strictly as an instrument for promoting learning, with the commons as the endpoint.
% FOUNDING_PROBLEM_CORROBORATION: Innovation economists corroborate from outside the benefiting parties that the underproduction problem for creative work is real; legal historians corroborate the anti-monopoly genealogy of the Statute of Anne and the Framers' instrument-means framing. Notably, no one attests from the public domain's own seat — the commons cannot speak — which is itself the absent-voice finding recorded above.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.53: the forward-looking bargain still prices a real incentive (moderate baseline), but retrospective term extension delivers works' control with zero incentive effect, and formalities abolition plus anti-circumvention liability widened what is held back — hence the upward series from 0.24 to 0.53. Suppression (0.48 end-state) is authored as a RAW structural property — the engine scales only extractiveness by directionality and scope; the suppression_requirement series tracks enforcement-capacity change specifically: a step-up around t=30 (DMCA anti-circumvention machinery, takedown infrastructure) followed by a plateau once that machinery matured, which is why the series rises then flattens rather than tracking the extractiveness curve. Theater_ratio rises 0.15 to 0.43: copyright notices are legally unnecessary ritual post-Berne, and the 'promote progress' rationale offered for retrospective extensions is performative by this reading's own instrumental logic (nothing not-yet-created is incentivized). Accessibility_collapse is 0.40 — alternatives (open licensing, open access, patronage and subscription funding, public-domain sources) remain workable, so the arrangement does not foreclose exit the way a natural limit or a closed snare would. Resistance is 0.45 — sustained but channelled: constitutional challenge (Eldred), copyleft construction, open-access advocacy, and widespread informal noncompliance. All three tracked series run on ONE shared seven-point grid (t=0,10,20,30,40,50,55) with every metric authored at every point; the trajectory is monotonic drift, not cyclical, so no oscillation analysis applies. End-state scalars equal the t=55 series values by construction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the commercial_rights_holders seat the arrangement presents as an earned asset regime: they paid for creation, the window is the return, and longer terms read as ordinary property security — low experienced burden, high experienced legitimacy. From the public, downstream-creator, and future-generation seats the same structure presents as steadily delayed commons: every extension is a cost imposed without a corresponding new incentive. From the congress seat it presents as a discretionary dial with concentrated support on one setting. From the authors' dual seat it presents as both sides of one clock — collection now, contribution later — which is why that seat is genuinely ambivalent rather than aligned with either pole. The engine computes these divergences from the structural data; the authored scaffold claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality (subsidized seats): the commons, the reading public, libraries, downstream creators, and in-term rights-holders all collect from the arrangement's operation. No victims are declared — deliberately, because this reading's core claim is that the arrangement HAS no victims: in-term access restrictions are the temporary, published price of the commons enrichment, not extraction from a captive class. The excluded seats (future generations, orphan-work users) bear real diffuse costs, but under the R3 ruling an authored absence is commentary-grade only and must not drive classification overrides, so they inform the analysis without entering the derivation. No directionality_overrides are authored: the one candidate was congress_legislature, which accrues indirect political rents from the arrangement's persistence and therefore sits slightly below symmetric; but the override surface is keyed by power atom, and an institutional-atom override would also misstate commercial_rights_holders (whose beneficiary-derived low d is correct) and the court. The canonical fallback's near-symmetric treatment of the legislature is an accepted approximation, documented here rather than forced through a blunt override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — underproduction of creative work absent exclusive rights — is live, so no mandatrophy_resolved declaration is authored and the R5 mismatch consumer reads status=live x verdict=world_rearranges, producing no zombie flag. What HAS decayed is not the mandate but the sunset discipline: a scaffold whose defining feature is transition persists by serially postponing its own transition, and each postponement moves its operation toward the perpetual-enclosure condition the mandate was written to prevent. The classification apparatus keeps this distinct from two errors: labeling the arrangement a pure coordination rope (which would ignore the accumulating windfall component measured in the extractiveness series) and labeling it a snare (which would ignore that the bargain's coordination function is real, its beneficiaries numerous, and its endpoint — release — still structurally encoded). The theater_ratio trajectory is the leading indicator of the degradation: as the incentive rationale detaches from actual term policy, the share of performative maintenance grows, and a scaffold maintained mostly by performance is the piton-adjacent failure mode this reading's own tradition warns against.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the copyright_constitutional_mandate kernel (public_scaffold_reading). Which reading captures the operative legal understanding — this one, corporate_enclosure_reading (maximal protection, ''limited'' meaning short of perpetuity), or judicial_ambiguity_reading (unreviewable legislative discretion)?',
    'Doctrinal trajectory analysis: whether courts or legislatures ever enforce an incentive-tether or a release requirement, versus treating term length as purely discretionary property policy. Sibling files carry the same omega from their own seats.',
    'If the enclosure reading becomes operative, the beneficiary structure inverts (rights-holders become near-perpetual owners, the commons a residual) and effective extraction rises sharply; if the ambiguity reading holds, this reading''s charter constrains nothing and its metrics describe aspiration, not operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the copyright kernel is structurally operative.').

omega_variable(
    epsilon_decomposition_incentive_vs_windfall,
    'How much of the measured extractiveness comes from the incentive price of the forward-looking bargain (coordination cost) versus the windfall component of retrospective extension and formalities-free automatic terms (pure holding-back of paid-for works)?',
    'Economic decomposition of term value by cohort: compare the private value of terms on newly created works against the value of extensions applied to existing catalogs, using licensing-market data and the Eldred-era legislative record.',
    'If windfall-dominated, effective extraction is materially higher than the scalar suggests and scaffold certification fails on the excess-extraction side; if incentive-priced, the arrangement sits closer to its charter and the drift reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_decomposition_incentive_vs_windfall, empirical, 'Split of measured extraction between incentive price and retrospective windfall.').

omega_variable(
    treaty_lock_on_restoration,
    'How binding are Berne/TRIPS minimum terms and formalities prohibitions on any domestic attempt to shorten terms or restore registration formalities — is the restoration path this reading requires actually available?',
    'Treaty-law analysis of withdrawal/renegotiation precedents and of grandfathering space for formalities-style opt-in registries; observe whether any jurisdiction has successfully shortened terms.',
    'If the lock is soft, fixing_cost drops toward cheap and the scaffold''s sunset becomes practically reachable; if hard, the sunset clause is nominal and the arrangement''s transitional character is effectively extinguished regardless of charter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_lock_on_restoration, empirical, 'Whether treaty commitments make the mandated transition practically unreachable.').

omega_variable(
    commons_as_nonagent_beneficiary,
    'The public domain is authored as a non-agent beneficiary (agent=false, excluded from directionality). Does the coordination-function case for this reading survive if the commons is treated as mere background with no beneficiary seat at all?',
    'Re-run the classification with the commons removed from the beneficiary set: if the coordination gate still closes on the human beneficiary seats (creators, public, libraries), the framing choice is immaterial; if it fails, the reading''s classification depends on personifying an abstraction.',
    'If the scaffold classification depends on the non-agent seat, the reading''s structure is weaker than authored and the arrangement may compute as unanchored enforcement rather than transitional coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_as_nonagent_beneficiary, conceptual, 'Framing dependence of the coordination case on the personified commons.').

omega_variable(
    term_policy_values_question,
    'Should term lengths be re-tethered to empirical incentive evidence, and who is entitled to decide — this is ultimately a values question about the weight owed to future users versus present rights-holders?',
    'Not resolvable by data alone: resolved by legislative choice, constitutional interpretation, or treaty politics. Empirical inputs (the incentive-cohort decomposition above) inform but do not settle it.',
    'Preference resolution changes the feasibility and desirability of the restoration path and therefore the practical meaning of fixing_cost; it does not change the structural facts authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_policy_values_question, preference, 'The irreducible policy-values question underlying term length.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(copy_tr_t0, observed).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(copy_tr_t10, observed).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(copy_tr_t20, observed).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(copy_tr_t30, observed).
narrative_ontology:measurement(copy_tr_t40, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(copy_tr_t40, observed).
narrative_ontology:measurement(copy_tr_t50, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(copy_tr_t50, observed).
narrative_ontology:measurement(copy_tr_t55, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 55, 0.43).
narrative_ontology:measurement_basis(copy_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(copy_be_t0, observed).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement_basis(copy_be_t10, observed).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(copy_be_t20, observed).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(copy_be_t30, observed).
narrative_ontology:measurement(copy_be_t40, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(copy_be_t40, observed).
narrative_ontology:measurement(copy_be_t50, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement_basis(copy_be_t50, observed).
narrative_ontology:measurement(copy_be_t55, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 55, 0.53).
narrative_ontology:measurement_basis(copy_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(copy_su_t0, observed).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(copy_su_t10, observed).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(copy_su_t20, observed).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(copy_su_t30, observed).
narrative_ontology:measurement(copy_su_t40, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement_basis(copy_su_t40, observed).
narrative_ontology:measurement(copy_su_t50, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement_basis(copy_su_t50, observed).
narrative_ontology:measurement(copy_su_t55, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 55, 0.48).
narrative_ontology:measurement_basis(copy_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'copyright' (kernel: copyright_constitutional_mandate). The label conflates three structurally distinct constraints that share one constitutional text but differ in beneficiary structure, epsilon, and type: this file (public_scaffold_reading — transitional coordination, commons as terminal beneficiary, low-to-moderate epsilon, scaffold charter), corporate_enclosure_reading (property-right maximalism, rights-holders as dominant beneficiaries, contested epsilon), and judicial_ambiguity_reading (deference regime, no strong beneficiary declaration, epsilon set by whatever Congress enacts). The upstream story in empirical-confidence terms is the constitutional text itself; this reading cites the Framers' instrument-means language as evidence against the enclosure reading, and the ambiguity reading is downstream of BOTH as the institutional settlement their dispute produced. All three files link one another via network.affects_constraints; contamination propagates across the family because a shift in any one reading's operative status changes the legitimacy conditions of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
