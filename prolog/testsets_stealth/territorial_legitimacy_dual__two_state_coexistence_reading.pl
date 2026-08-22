% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Framework: Mutual Recognition at the 1967 Lines
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   The two-state coexistence reading instantiates the
 *   territorial_legitimacy_dual kernel as a reciprocal-recognition bargain:
 *   both peoples' 1948 legitimacy claims are accepted, the 1967 lines (with
 *   agreed swaps) form the partition basis, the refugee return claim is
 *   satisfied within the Palestinian state rather than pre-1948 localities,
 *   and security cooperation replaces zero-sum rivalry. The framework solves
 *   a real collective-action problem — two peoples, one territory, neither
 *   able to secure exclusive legitimacy without perpetual war — while
 *   imposing concentrated costs on identifiable minorities: settler
 *   communities beyond the blocs, whose project the boundary forecloses, and
 *   the refugee diaspora, whose inherited claim the return-limitation
 *   extinguishes in exchange for compensation and citizenship elsewhere. This
 *   story is one member of a three-story constraint family decomposing the
 *   colloquial label 'the two-state solution'; the siblings
 *   (zionist_refuge_reading, palestinian_autochthony_reading) instantiate
 *   different remedies over the same legitimacy premise and carry different
 *   epsilon values and victim sets. Claim and metrics are authored
 *   independently: the claimed type is tangled_rope because the structure
 *   possesses both a genuine coordination function and asymmetric
 *   concentrated payment under active enforcement; the metrics describe the
 *   framework's actual operation across 1967–2024, including a theater drift
 *   the claim does not reconcile away.
 *
 * KEY AGENTS:
 *   - - israeli_mainstream_public: Primary beneficiary (organized/constrained) — gains recognized borders, normalization, and a security partner; bears implementation upheaval indirectly
 *   - - palestinian_mainstream_public: Primary beneficiary (moderate/constrained) — gains statehood and end of occupation; accepts return-limitation and demilitarization as the price
 *   - - guarantor_powers: Secondary beneficiary and agenda-setter (institutional/arbitrage) — sponsor, fund, and underwrite the framework; gain stability and agenda control; can disengage cheapest
 *   - - west_bank_settlers_outside_blocs: Primary payer (organized/identity_locked) — communities slated for evacuation or transfer of sovereignty; the framework forecloses their constitutive project
 *   - - palestinian_refugee_diaspora: Primary payer (powerless/trapped, generational horizon) — millions whose return claim is converted to compensation and citizenship in the new state; consent mediated by others
 *   - - palestinian_rejectionist_factions: Excluded actor (organized/trapped) — deliberately kept outside the process because participation would veto the bargain; contest it from outside
 *   - - quartet_envoys: Analytical observer (institutional/analytical) — mediate, draft roadmaps, certify parameters; hold no territorial stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.5).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework: Mutual Recognition at the 1967 Lines").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '004195ef-5e23-4e60-88c9-271b50171d2a').
narrative_ontology:cs_kernel_codification('004195ef-5e23-4e60-88c9-271b50171d2a', formalized).
narrative_ontology:cs_authority_grounding('004195ef-5e23-4e60-88c9-271b50171d2a', distributed).
narrative_ontology:cs_reading_relation('004195ef-5e23-4e60-88c9-271b50171d2a', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('004195ef-5e23-4e60-88c9-271b50171d2a', territorial_legitimacy_dual__palestinian_autochthony_reading, influences).
narrative_ontology:cs_axiom('004195ef-5e23-4e60-88c9-271b50171d2a', foundational, dual_legitimacy_mutual_recognition).
narrative_ontology:cs_axiom_status(dual_legitimacy_mutual_recognition, holdable).
narrative_ontology:cs_axiom_grounding('004195ef-5e23-4e60-88c9-271b50171d2a', dual_legitimacy_mutual_recognition, deontological).
narrative_ontology:cs_axiom('004195ef-5e23-4e60-88c9-271b50171d2a', foundational, return_limited_to_palestinian_state).
narrative_ontology:cs_axiom_status(return_limited_to_palestinian_state, holdable).
narrative_ontology:cs_axiom_grounding('004195ef-5e23-4e60-88c9-271b50171d2a', return_limited_to_palestinian_state, instrumental).
narrative_ontology:cs_reference_frame('004195ef-5e23-4e60-88c9-271b50171d2a', mutual_recognition_1967_compact).
narrative_ontology:cs_drift_state('004195ef-5e23-4e60-88c9-271b50171d2a', contemporary_post_oslo_collapse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('004195ef-5e23-4e60-88c9-271b50171d2a', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_mainstream_public).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_mainstream_public).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, guarantor_powers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settlers_outside_blocs).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, land_for_peace_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, mutual_recognition_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, unsc_242_lineage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elects the governments and sustains the coalitions that would sign a final-status agreement. Gains recognized borders, normalized regional standing, and a security-cooperation partner in place of open-ended garrison duty. Bears the agreement's costs indirectly: the political upheaval of evacuating settlements and the surrender of the heartland claim that parts of its own electorate hold sacred. Leaving the territory-question entirely is not available; emigration exists but does not settle the collective stake.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_mainstream_public, beneficiary,
    organized, biographical, constrained, national).

% Would receive a sovereign state on the 1967 line with a shared or divided Jerusalem, an end to military occupation, and defined citizenship. Pays by accepting that the refugee claim resolves inside the new state rather than in pre-1948 towns, and by accepting demilitarization and security-coordination terms that constrain the new state's sovereignty from its first day. There is no alternative venue offering statehood on comparable terms.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_mainstream_public, beneficiary,
    moderate, biographical, constrained, regional).

% The United States, European Union members, Egypt, Jordan, and Gulf states sponsor, fund, and underwrite the framework through aid, monitoring missions, and diplomatic cover. They gain regional stability, alliance discipline, and a settled issue removed from the security agenda. They set the diplomatic agenda through funding conditionality and Security Council drafting, and they can redirect sponsorship or disengage at far lower cost than any party inside the territory.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, guarantor_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, guarantor_powers, agenda_setter).

% Live in communities beyond the separation barrier and outside the blocs slated for retention under land-swap formulas. A final-status line means evacuation, dissolution of communities built over decades, or remaining under Palestinian sovereignty — each experienced as the end of a religious-national project rather than a relocation decision. The 2005 Gaza disengagement is the lived precedent shaping total opposition. The communities are not assets held but identities inhabited, so exit is not meaningfully available.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settlers_outside_blocs, payer,
    organized, biographical, identity_locked, local).

% Millions registered with UNRWA across Lebanon, Jordan, Syria, and beyond, many stateless or holding second-class host-state status. The framework converts their inherited claim to return into compensation, citizenship in the new Palestinian state, and symbolic acknowledgment. Their consent is mediated by negotiators they did not elect; camp isolation and host-state politics leave them no independent lever over the terms, and the claim passes across generations, compounding what is signed away.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, continental).

% Movements such as Hamas were deliberately kept outside the Oslo and final-status processes because their participation would veto the compromise. The exclusion is a design feature: the agreement's arithmetic works only if armed rejectionism is absent from the table. They contest the framework from outside it, and their capacity to strike the process has repeatedly reshaped it — each major escalation redrew what negotiators could offer.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_rejectionist_factions, excluded,
    organized, biographical, trapped, regional).

% The Quartet (United Nations, United States, European Union, Russia) and successive special envoys mediate, draft roadmaps, and certify the framework's parameters — 1967 lines, two capitals, agreed swaps, security arrangements. They hold no territorial stake; their seat exists to translate between the parties' framings and keep the parameters on the diplomatic record.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, quartet_envoys, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a zero-sum sovereignty contest into a mutually legible partition: fixed borders, mutual recognition of both states, and security cooperation solve the problem that neither people can secure exclusive legitimacy without perpetual war, and that neither can credibly guarantee the other's safety unilaterally.
% TRANSFER_FUNCTION: Moves territorial concessions (settlement evacuation, land swaps) from Israeli expansion constituencies; moves the return claim (from repatriation into Israel to citizenship in Palestine plus compensation) from the refugee diaspora; moves security guarantees, aid, and diplomatic cover from guarantor powers to both states.
% ABSENT_VOICES: The refugee diaspora's internal plurality — camp committees, host-state refugees, second-generation claimants — held no seat at Oslo or final-status tables; rejectionist factions were excluded by design; settler leadership was consulted late and performatively. Present, they would object that the compromise trades their constitutive claims for other people's stability, and that unanimity at the table existed partly because the strongest objectors were never admitted to the room.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the diplomatic architecture organized around it — the Resolution 242 lineage, aid conditionality, the Arab Peace Initiative's recognition offer, guarantor funding channels — loses its organizing reference point. Both societies lose the only mutually legible endpoint their moderates share, and the contest reverts to open-ended rivalry with no agreed vocabulary for ending it. The named seats' arrangements demonstrably depend on the framework's existence as reference, whatever its implementation status.
% FOUNDING_PROBLEM: Built to resolve the post-1967 crisis: a state holding occupied territories populated by a stateless people under military rule — incompatible with annexation (the demographic dilemma) and intolerable as status quo (perpetual insurgency) — compounded by the unresolved 1948 refugee question that blocked legitimation of either sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN Security Council resolutions and Secretary-General reporting document the continuing occupation and statelessness; the International Court of Justice's 2024 advisory proceedings attest unresolved sovereignty; host-state governments (Lebanon, Jordan) and UNRWA reporting attest the refugee problem's persistence; longitudinal polling in both societies documents continuing majority desire for an agreed resolution. None of these sources sits exclusively inside the framework's beneficiary set.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness settles near 0.5 because the framework's payments are formally reciprocal yet concentrated: the value surrendered by the settler and refugee seats is real, specific, and borne by minorities, while the returns (recognition, borders, statehood, stability) distribute broadly. Suppression (0.58) is structural: the bargain holds only if rejectionism is policed on both sides — demilitarization terms, security coordination, and the deliberate exclusion of veto-capable factions are load-bearing, not incidental. Accessibility_collapse (0.60) reflects that once the framework is accepted as the endpoint, maximalist alternatives collapse substantially in international standing without disappearing among constituencies. Resistance (0.70) is high and well-documented: every near-adoption (Oslo, Camp David 2000, Annapolis, the 2013–14 Kerry effort) drew armed or mass rejectionist response from both directions. The temporal series run on one shared grid (t=0,13,26,33,40,47,57 ≙ 1967, 1980, 1993, 2000, 2007, 2014, 2024) with all three tracked metrics authored at every point. Base extractiveness peaks at t=33 (2000) when final-status costs crystallized and both rejectionisms mobilized, then eases as implementation recedes and the payments become latent. Theater_ratio climbs monotonically from 0.15 to 0.60 — classic Goodhart drift: invoking the framework (communiqués, anniversary statements, roadmap language) progressively substitutes for implementing it, crossing the 0.5 proxy-substitution threshold after 2007. The suppression_requirement series is authored because enforcement capacity is the story's tracked dynamic: modest during the doctrinal era, building through Oslo implementation, spiking at 2000 (Second Intifada suppression, faction proscription, disengagement enforcement), then declining slowly as enforcement shifted from advancing the framework to merely maintaining its default status. Receipt surface: gain_flow is authored as 'diffuse' after checking every named seat — the conceded value splits between the two publics (borders and recognition to one, statehood and territory to the other) with stability rents accruing to guarantors as byproduct, not capture; no single seat receives the transfer. fixing_cost is 'prohibitive': the parameters are well known and repeatedly drafted, yet every fix attempt has failed at extreme political cost, so the cost class of fixing exceeds the benefit any single incumbent can collect.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the guarantor seat (arbitrage exit, generational horizon) the framework is a manageable portfolio item — a doctrine to fund, defer, or revive. From the two mainstream-public seats it is a fair trade: painful, reciprocal, legitimate. From the settler seat (identity_locked) the same structure is existential dispossession — the 2005 disengagement is the lived precedent that evacuation means the end of a world, not a relocation package. From the refugee seat (trapped, generational) it is betrayal by representation: a constitutive claim signed away by negotiators the diaspora did not elect. The engine derives these per-seat classifications from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the two mainstream publics hold low directionality (net subsidized, constrained exit), and the guarantor powers sit nearest the beneficiary pole (indirect benefit, arbitrage exit, agenda-setting leverage). Victim declarations drive the opposite end: settlers approach the full-target pole because identity_lock removes exit modulation — they cannot reprice or relocate their stake; the refugee diaspora sits near-full-target with trapped exit and a generational horizon that compounds the foreclosure across generations. The rejectionist factions are excluded rather than coordinated: their exclusion is the enforcement object itself, which is why suppression, not extraction, is the metric that tracks their seat. No directionality overrides are needed — the beneficiary/victim plus exit-option data already yields the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — two peoples, one territory, unresolved sovereignty and refuge — is live, so the mandate has not outlived its function and no mandatrophy declaration is authored. The classification prevents two symmetrical mislabelings: a pure-rope reading would erase the concentrated payers whose foreclosure is the framework's price (settlers and refugees are not friction costs but the payment itself); a pure-snare reading would erase the genuine coordination achievement — mutual recognition architecture is the only mutually legible endpoint either society's moderates have ever endorsed. The theater series is the early-warning instrument: theater_ratio above 0.5 since 2007 with no process attached is the signature of transition toward piton dynamics, and the default_status_viability omega routes that question to an explicit test rather than letting the current tangled_rope claim stand unexamined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the territorial_legitimacy_dual kernel. Where exactly do the three readings disagree — on the premise that both peoples hold legitimate claims, or on the remedy each assigns to that premise?',
    'Comparative analysis of the sibling stories'' beneficiary/victim sets and declared axioms: if the disagreement locates in remedy selection (scope of return, boundary basis, security architecture) while all three affirm dual legitimacy, the readings are remedy-rivals; if any sibling denies the dual-premise itself, the contest is premise-level.',
    'If the contest is remedy-level, the coexistence reading functions as a meta-framework capable of absorbing elements of both siblings, and hybrid packages remain structurally possible. If premise-level, the readings cannot share a single framework and foreclosure analysis between siblings becomes live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the kernel contest: premise-level versus remedy-level disagreement across the three readings.').

omega_variable(
    refugee_consent_validity,
    'Does the limitation of return to the Palestinian state carry the informed consent of the refugee diaspora itself, or is it a concession made by negotiating representatives optimizing for statehood on behalf of a constituency they did not fully consult?',
    'Representative surveying and referendum-grade consultation of refugee populations across host states; comparison of negotiator mandates with diaspora polling; camp-committee deliberation records.',
    'If consent fails, the framework''s heaviest concession falls on a non-consenting constituency and the tangled_rope balance shifts toward snare dynamics despite the genuine coordination function; if consent holds, the limitation reads as a legitimate term of trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_consent_validity, empirical, 'Whether the return-limitation concession is consensual for the group that bears it.').

omega_variable(
    concession_symmetry_assessment,
    'Are the reciprocal concessions symmetric in value — territory and recognition traded against return and statehood — or does the framework ask the weaker party to surrender its stronger claim while the stronger party surrenders its weaker one?',
    'Structured valuation comparing land-swap packages against actuarial and diplomatic valuation of return rights; negotiation-history analysis of which terms each side treated as red-lines versus tradable.',
    'A finding of asymmetry raises effective pressure on the Palestinian payer seats and supports corrective weighting (compensation scales, swap ratios, guarantor offsets); a finding of symmetry stabilizes the tangled_rope classification with balanced reciprocal payment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_symmetry_assessment, preference, 'Symmetry of the reciprocal concessions across the two payer seats.').

omega_variable(
    default_status_viability,
    'Is the framework''s persistence as the sole internationally recognized endpoint evidence of a live coordination function awaiting implementation, or of inertial invocation whose substantive function has drained away?',
    'Counterfactual test: observe whether a credible restart (renegotiation, renewed guarantor investment) revives substantive compliance behavior on both sides, versus continued ceremonial reaffirmation without process.',
    'Revival confirms a tangled_rope with recoverable coordination function; continued ceremony-only persistence supports reclassification toward piton dynamics, with the framework maintained as diplomatic liturgy while its referent recedes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_status_viability, empirical, 'Whether the framework''s default status reflects live function or inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t13, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 13, 0.22).
narrative_ontology:measurement_basis(terr_tr_t13, observed).
narrative_ontology:measurement(terr_tr_t26, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 26, 0.35).
narrative_ontology:measurement_basis(terr_tr_t26, observed).
narrative_ontology:measurement(terr_tr_t33, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 33, 0.48).
narrative_ontology:measurement_basis(terr_tr_t33, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t47, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 47, 0.58).
narrative_ontology:measurement_basis(terr_tr_t47, observed).
narrative_ontology:measurement(terr_tr_t57, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 57, 0.6).
narrative_ontology:measurement_basis(terr_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t13, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 13, 0.44).
narrative_ontology:measurement_basis(terr_be_t13, observed).
narrative_ontology:measurement(terr_be_t26, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 26, 0.5).
narrative_ontology:measurement_basis(terr_be_t26, observed).
narrative_ontology:measurement(terr_be_t33, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 33, 0.58).
narrative_ontology:measurement_basis(terr_be_t33, observed).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t47, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 47, 0.53).
narrative_ontology:measurement_basis(terr_be_t47, observed).
narrative_ontology:measurement(terr_be_t57, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 57, 0.5).
narrative_ontology:measurement_basis(terr_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t13, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 13, 0.34).
narrative_ontology:measurement_basis(terr_su_t13, observed).
narrative_ontology:measurement(terr_su_t26, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 26, 0.38).
narrative_ontology:measurement_basis(terr_su_t26, observed).
narrative_ontology:measurement(terr_su_t33, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 33, 0.68).
narrative_ontology:measurement_basis(terr_su_t33, observed).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t47, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 47, 0.61).
narrative_ontology:measurement_basis(terr_su_t47, observed).
narrative_ontology:measurement(terr_su_t57, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 57, 0.58).
narrative_ontology:measurement_basis(terr_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the two-state solution' covers three structurally distinct constraints instantiating one kernel (territorial_legitimacy_dual). Per the epsilon-invariance principle they are authored as separate stories: this coexistence reading carries epsilon ~0.50 over a reciprocal-concession structure with concentrated payers (settlers, refugees); the zionist_refuge_reading authors epsilon for an arrangement weighted toward Israeli sovereignty (different victim set: Palestinian self-determination constituencies); the palestinian_autochthony_reading authors epsilon for an arrangement centered on return (different victim set: Israeli demographic-sovereignty constituencies). The coexistence reading sits mid-stream in the family: it absorbs both siblings' legitimacy premises and exerts structural pressure back on their maximalist forms, which is why its edges are declared 'influences'. Each story links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
