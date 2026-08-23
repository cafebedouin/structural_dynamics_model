% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Post-Nakamoto Interpretive Vacuum: Whitepaper as Contested Substrate
 *   domain: distributed systems / monetary theory / technology governance
 *
 * SUMMARY:
 *   When the pseudonymous author of the Bitcoin paper went silent in 2011,
 *   the protocol lost the only party who could have certified its purpose,
 *   and the nine-page text became contested substrate: a fixed artifact that
 *   every subsequent faction reads as vindicating its own program. This story
 *   authors THAT arrangement — the standing no-oracle interpretive regime —
 *   as a single ε-invariant constraint, per the decomposition discipline. The
 *   colloquial question 'what did Satoshi intend?' resolves into three
 *   structurally distinct stories: this one (the interpretive vacuum itself),
 *   plus the electronic_cash and store_of_value readings, which are separate
 *   files linked through the network section. The claim/metric gap here is
 *   deliberate: the reading is CLAIMED as tangled_rope because the vacuum
 *   genuinely coordinates (capture-resistance, no key-person dependency)
 *   while simultaneously extracting (unfalsifiable fidelity claims harvest
 *   legitimacy; the commons pays dispute and split costs). KEY AGENTS (by
 *   structural relationship): - reference_client_maintainers: de facto agenda
 *   setter (organized / identity_locked) — inherited interpretive discretion
 *   through code stewardship; - fidelity_claim_factions: primary beneficiary
 *   (organized / mobile) — both major camps harvested legitimacy from
 *   unclosable citations; - mining_pool_operators: second agenda setter
 *   (institutional / arbitrage) — signaling weight plus frictionless
 *   redeployment; - merchant_payment_processors: payer (powerful /
 *   constrained) — bore congestion and stalemate costs, bound by liquidity; -
 *   transacting_users: payer with offsetting protection benefit (powerless /
 *   constrained) — fee spikes versus founder-proof custody; -
 *   minority_fork_communities: payer who exited (moderate / mobile) — carried
 *   split costs to keep a reading alive; - authority_claimants: excluded
 *   (powerful / trapped) — the enforcement object is keeping them outside
 *   legitimacy; - governance_scholars: analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Post-Nakamoto Interpretive Vacuum: Whitepaper as Contested Substrate").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed systems / monetary theory / technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'aa6ad496-87b5-4e0b-a150-e2c26e107037').
narrative_ontology:cs_kernel_codification('aa6ad496-87b5-4e0b-a150-e2c26e107037', fixed_text).
narrative_ontology:cs_authority_grounding('aa6ad496-87b5-4e0b-a150-e2c26e107037', distributed).
narrative_ontology:cs_reading_relation('aa6ad496-87b5-4e0b-a150-e2c26e107037', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_reading_relation('aa6ad496-87b5-4e0b-a150-e2c26e107037', bitcoin_whitepaper_purpose__store_of_value_reading, influences).
narrative_ontology:cs_axiom('aa6ad496-87b5-4e0b-a150-e2c26e107037', foundational, no_binding_interpretive_oracle_exists).
narrative_ontology:cs_axiom_status(no_binding_interpretive_oracle_exists, holdable).
narrative_ontology:cs_axiom_grounding('aa6ad496-87b5-4e0b-a150-e2c26e107037', no_binding_interpretive_oracle_exists, conventional).
narrative_ontology:cs_axiom('aa6ad496-87b5-4e0b-a150-e2c26e107037', foundational, founder_absence_is_constitutive).
narrative_ontology:cs_axiom_status(founder_absence_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('aa6ad496-87b5-4e0b-a150-e2c26e107037', founder_absence_is_constitutive, instrumental).
narrative_ontology:cs_axiom('aa6ad496-87b5-4e0b-a150-e2c26e107037', secondary, legitimacy_flows_from_running_code).
narrative_ontology:cs_axiom_status(legitimacy_flows_from_running_code, holdable).
narrative_ontology:cs_axiom_grounding('aa6ad496-87b5-4e0b-a150-e2c26e107037', legitimacy_flows_from_running_code, conventional).
narrative_ontology:cs_reference_frame('aa6ad496-87b5-4e0b-a150-e2c26e107037', authorless_contested_substrate).
narrative_ontology:cs_drift_state('aa6ad496-87b5-4e0b-a150-e2c26e107037', contemporary_post_identity_litigation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aa6ad496-87b5-4e0b-a150-e2c26e107037', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fidelity_claim_factions).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, reference_client_maintainers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, merchant_payment_processors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, transacting_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, minority_fork_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, transacting_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold merge access to the dominant reference implementation and steward the improvement-proposal process. Since the founder's departure they publicly disclaim leadership while deciding which protocol changes reach activation, exercising discretion through code review and activation thresholds rather than decree. They cannot compel anyone to run their software, and every attempt to formalize their authority has been rebuffed by the wider participant base. Leaving would mean abandoning a decade of accumulated context and standing.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, reference_client_maintainers, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, reference_client_maintainers, beneficiary).

% Rival camps in successive protocol disputes — larger blocks versus tighter decentralization budgets, inscription-tolerant versus purist positions — each citing the same nine-page paper as warrant. Because no living party can certify the author's intent, a fidelity claim cannot be falsified, which is precisely what makes the citation valuable. A camp that loses a contest can fork and continue under its own banner, as one large camp did in 2017.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fidelity_claim_factions, beneficiary,
    organized, biographical, mobile, global).

% Aggregate hashrate on behalf of thousands of connected miners, signal support for proposed rule changes, and can redirect equipment between compatible chains at negligible cost. Short reward cycles make them acutely attentive to fee conditions and chain-split risk. Their signaling weight grew as disputes persisted, and they brokered several attempted settlements during the capacity wars.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, mining_pool_operators, agenda_setter,
    institutional, immediate, arbitrage, global).

% Businesses built on low-cost, predictable settlement: payment processors, exchanges, commercial service firms. Congestion episodes raised their unit costs and reconciliation burdens, and multi-year upgrade stalemates delayed product roadmaps. Supporting a rival chain would mean splitting liquidity and duplicating compliance work, so they remained on the dominant asset throughout while lobbying openly for capacity relief.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, merchant_payment_processors, payer,
    powerful, biographical, constrained, global).

% People who hold and move the asset. During congestion windows they paid sharply higher fees and waited longer for confirmation; across the same period their savings sat protected from any founder-level takeover, lawsuit, or regulatory action aimed at a named leader. Most lack technical standing in protocol debate; their leverage surfaces collectively, as when node operators signaled readiness to enforce rules a miner majority opposed in 2017.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, transacting_users, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, transacting_users, beneficiary).

% Communities that split off after losing a protocol contest and now maintain a parallel ledger descending from the same genesis block. They carry persistent replay-attack exposure at split boundaries, reduced network effects, and the continuing cost of defending a minority narrative; in exchange they keep their preferred reading of the text alive without anyone's permission.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, minority_fork_communities, payer,
    moderate, biographical, mobile, global).

% Individuals who have asserted the founder's identity or claimed standing to settle what the text means: self-declared successors, litigation-backed claimants, builders of would-be foundations. The community demands cryptographic proof, then withholds recognition even when documents and courtroom proceedings occur, and a 2024 judicial finding went unanswered by any shift in social acceptance. Their exclusion is maintained rather than incidental.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, authority_claimants, excluded,
    powerful, biographical, trapped, global).

% Researchers in protocol governance, monetary history, and open-source maintenance who document the fork wars, forum-moderation controversies, and identity disputes. They take no side and bear none of the costs; every camp cites their work selectively.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, reference_client_maintainers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The absence of any authorized interpreter removes the single most attractive attack surface in the system: there is no founder to capture, subpoena, coerce, discredit, or simply grow old. Protocol disputes must be settled by consent among economic actors rather than by decree, and the boundary of 'Bitcoin' is maintained by what nodes choose to validate rather than by what an authority declares.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and agenda-setting discretion away from any would-be central authority and toward whichever coalition prevails in each consensus contest, with a durable residual share accruing to stewards of the dominant implementation; moves the costs of unresolved dispute — negotiation overhead, split risk, fee volatility — onto the entire participant base.
% ABSENT_VOICES: Would-be authoritative interpreters are excluded by design; their objections to the vacuum cannot be voiced from inside legitimacy at all. Non-technical holders had thin representation in mailing-list and IRC-era debates where norms froze, and future cohorts of users were absent when the interpretive constitution settled. The departed founder himself is absent in the literal sense; whether he would clarify anything is permanently unverifiable.
% DISAPPEARANCE_RATIONALE: If an authoritative interpreter suddenly existed — a cryptographically proven originator issuing binding clarifications, or an institution the participant base agreed to obey — the fork wars' driving indeterminacy would collapse: fidelity claims would become falsifiable, minority chains built on rival readings would lose their reason for being, the reference implementers' informal discretion would end, and a decade of 'the text means what we won' rhetoric would lose its audience.
% FOUNDING_PROBLEM: A system whose entire premise was the removal of trusted third parties could not credibly retain its own author as final arbiter. The arrangement traces to the founder's deliberate wind-down — handing off alert keys, announcing he had 'moved on to other things,' and going silent in 2011 — so that the protocol would have to stand on consensus rather than on a person.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any current benefiting camp: the founder's own handover communications establish the withdrawal as intentional; independent histories of the protocol and of open-source governance treat founder-exit as a designed property; and, decisively, rival camps that would gain enormously from a friendly oracle nonetheless refuse to recognize any claimant — the 2016–2024 identity-litigation saga ended in judicial findings and continued social rejection alike. No party, including the camps this arrangement disadvantages, has acted to install a replacement oracle.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 terminal) is moderate-to-substantial: the vacuum converts interpretive closure into a free input for legitimacy claims — any camp can assert whitepaper fidelity at zero falsification risk — while dispute overhead, fee volatility during contested activations, and split costs land on participants who never chose to adjudicate anything. Suppression (0.42) is real but bounded: rival interpretations are not banned, they are starved — network effects, hashrate economics, and social rejection punish minority readings after the fact, and forum-moderation controversies in the mid-2010s plus the long identity-litigation saga show the no-oracle condition is actively defended rather than merely suffered. Theater (0.50) is high because much of the constraint's observable activity is ritual: anniversaries, genesis-block recitations, 'Satoshi's vision' branding (one fork took the phrase as its name), and ceremonial whitepaper quotation that legitimates rather than interprets. Accessibility_collapse is moderate (0.40): understanding the vacuum does not exhaust your options — fighting via consensus politics, forking, or building elsewhere all remain available, as the 2017 split demonstrated. Resistance is substantial (0.60): the record shows repeated, expensive attempts to end the vacuum — self-declared successors, foundation proposals, brokered industry agreements, litigation — each defeated, each proving the equilibrium's durability. All three temporal series run on one shared grid (2011–2025, eight points); the trajectory is a conflict cycle rather than monotonic drift: accumulation → crisis (2015–2017 capacity war) → partial settlement → renewed contestation (2023 inscription disputes), with extraction peaking at the 2017 fork and settling slightly above pre-war levels thereafter. The oscillation is partly functional (contests periodically burn off accumulated interpretive debt) and partly an intermittent-reinforcement dynamic: each unresolved cycle teaches factions that patience plus a fresh fidelity claim can reopen a closed question.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute opposite constraints from identical raw material. From inside a fidelity camp, the vacuum is freedom: the text cannot be used against you, defeat is never final, and every fork is a fresh hermeneutic jurisdiction. From the merchant and user seats, the same vacuum is an unappealable tribunal that never adjourns — every upgrade becomes a referendum with their fee schedules and settlement guarantees as stakes. The maintainer seat adds a third experience: discretionary burden without authority — they absorb blame for outcomes no one empowered them to cause, and their identity fusion with stewardship ('we are not in charge') is precisely what prevents them from converting de facto agenda-setting into de jure closure. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Fidelity_claim_factions sit nearest the beneficiary end: they receive legitimacy transfers funded by the commons' dispute costs, and their mobile exit (fork) means even defeat is subsidized — losing produces a new sovereign territory. Reference_client_maintainers derive low d through the beneficiary declaration, tempered by real borne costs (harassment, blame absorption during congestion) that keep them short of full subsidy. Merchant_payment_processors are near-full targets: they paid the war's bills, and liquidity lock-in denies them arbitrage-grade exit. Minority_fork_communities bore the largest discrete costs (split, replay risk, permanent marginalization), but their mobile exit dampens effective extraction relative to trapped payers. Transacting_users carry a genuinely mixed position — congestion losses on one side, founder-proof custody on the other — which the structural derivation from the bare 'victim' declaration would misread as full-target; a directionality override (powerless → d=0.45) corrects this, since no other seat shares the powerless atom. Mining_pool_operators are near-symmetric (override institutional → 0.5): they profit from fee-market resolution under any reading and lose from splits under any reading, and their arbitrage-grade exit caps effective extraction. Authority_claimants are outside the transfer entirely — they are what the enforcement machinery excludes, not whom it taxes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — key-person dependency in a trust-minimization system — is live, not dead: every identity-claim episode demonstrates that an accepted oracle would instantly become the system's most valuable attack surface. Because the founding problem is live and the arrangement tracks it, there is no zombie mandate here, and the (founding_problem_status=live × world_rearranges) cell raises no capture flag. The tangled_rope classification is what prevents mislabeling in both directions: reading the vacuum as pure coordination (rope) would erase the identifiable payers — merchants who financed the stalemate, users who paid congestion rents, minority communities that bought their exit — while reading it as pure extraction (snare) would erase the documented protection value that even the payers concede. The theater_ratio's honest authoring matters because ritual citation is doing real maintenance work: the performances are how the equilibrium reproduces itself between crises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story is one reading of kernel bitcoin_whitepaper_purpose (reading: nakamoto_oracle_opacity). What would the sibling readings change structurally, and where exactly does the disagreement sit?',
    'Read alongside the electronic_cash_reading and store_of_value_reading stories: under the cash reading the binding arrangement is capacity rationing (victims are fee-paying transactors); under the store-of-value reading capacity restraint is itself the coordination good. The disagreement is located in whether the whitepaper''s title telos constrains protocol evolution at all — a question no surviving evidence can settle.',
    'Each reading yields a different constraint with its own ε, beneficiaries, and victims over the same underlying protocol history; cross-reading comparisons that pool them measure a chimera. This file''s ε covers ONLY the no-oracle arrangement, not capacity policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this constraint is one of three readings of a contested kernel; siblings relocate the victim set and the binding clause.').

omega_variable(
    oracle_return_counterfactual,
    'If the originator (or a cryptographically proven equivalent) returned and issued a binding clarification that the participant base accepted, would the vacuum close and how would classification redistribute?',
    'A signed statement meeting the community''s own proof standard, followed by observed acceptance behavior. Historical base rate is unfavorable: every prior claim failed the acceptance step regardless of documentary or judicial support.',
    'Acceptance would convert contested substrate into adjudicated kernel: fidelity-claim arbitrage collapses overnight (ε for faction seats drops), any camp defying the oracle becomes the new target class, and this reading approaches obsolescence. Rejection would confirm the vacuum as preference-independent rather than evidence-starved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_return_counterfactual, empirical, 'Whether a restored oracle is structurally possible or the vacuum has outlived its founder irreversibly.').

omega_variable(
    shield_or_rent_machine,
    'Is the no-oracle condition primarily protective coordination (capture-resistance, key-person-risk elimination) or primarily a subsidy machine for unfalsifiable legitimacy claims?',
    'Comparative governance analysis across systems with and without designated final interpreters (benevolent-dictator projects versus authorless protocols): measure capture incidents, dispute-resolution latency, and split frequency per capita of contentious decision.',
    'If the protective function dominates, the constraint trends toward rope and the measured excess extraction shrinks toward coordination cost; if the rent function dominates, it trends toward snare and the payers'' account becomes the primary description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shield_or_rent_machine, conceptual, 'Which of the vacuum''s two faces is structurally primary.').

omega_variable(
    joint_causation_with_capacity_axis,
    'How much of the measured extraction belongs to the interpretive vacuum versus to the underlying capacity scarcity that the sibling stories own?',
    'Counterfactual partition: would the 2015–2017 war have occurred with identical technical parameters but a living, accepted oracle? Historical evidence suggests the war was jointly produced — scarcity supplied the motive, the vacuum removed the referee — so ε must be apportioned, not assigned wholly to either story.',
    'If the vacuum merely amplified a conflict that scarcity alone would have produced, this story''s ε drops materially and the difference migrates to the electronic_cash_reading''s ledger; if the vacuum was load-bearing (an oracle would have settled it cheaply), this story retains the larger share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(joint_causation_with_capacity_axis, conceptual, 'Apportionment of measured extraction between this constraint and its capacity-policy sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 2011, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2011, 0.25).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2013, 0.3).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2017, 0.58).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2019, 0.55).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2021, 0.5).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2023, 0.53).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2025, 0.5).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2011, 0.3).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2013, 0.34).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2017, 0.56).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2023, 0.54).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2011, 0.15).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2013, 0.2).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2017, 0.46).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2019, 0.44).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2023, 0.43).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the whitepaper's purpose' decomposes into three ε-invariant constraints per the decomposition discipline: the electronic_cash_reading (telos-binding capacity policy), the store_of_value_reading (verifiability-first capacity policy), and this story (the interpretive vacuum that keeps both siblings permanently unresolved). The vacuum is upstream in legitimacy terms: it is the reason neither sibling can be ratified or refuted, and fork exit — the vacuum's signature affordance — is what preserved each sibling's losers as ongoing communities. Each file links the other two; ε values differ across the family because the referent differs (capacity arrangement versus interpretive arrangement), not because the observables were chosen differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, powerless, 0.45).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
