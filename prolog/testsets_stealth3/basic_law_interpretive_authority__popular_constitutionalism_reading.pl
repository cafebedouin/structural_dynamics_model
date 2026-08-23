% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Distributed Interpretive Authority Without Terminal Adjudication
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   In a constitutional order run on popular-constitutionalist principles, no
 *   institution — court or legislature — holds the last word on what the
 *   constitution means. Interpretive authority is deliberately distributed
 *   across social movements, elections, legislatures, courts, and civic
 *   associations, and fundamental law stays answerable to recurring public
 *   contestation. The arrangement coordinates a large polity's constitutional
 *   self-government while imposing real costs: institutions built to deliver
 *   settled answers never fully deliver them, judicial craft is discounted
 *   against mobilized public judgment, and planning horizons shorten for
 *   everyone who needs stable rules. This file instantiates ONE reading of
 *   the basic-law-interpretive-authority kernel (see kernel_context); the
 *   sibling readings are separate constraint files, and epsilon here is
 *   authored only for the standing popular-constitutionalist arrangement as
 *   its own lights assess it — never hedged or averaged across readings. The
 *   claim and the metrics are independent authored facts: claimed_type is
 *   tangled_rope because the structure genuinely coordinates while genuinely
 *   extracting; the metric values describe the arrangement's observed
 *   operation and were not tuned to any predicted engine verdict. KEY AGENTS
 *   (by structural relationship): - grassroots_constitutional_movements:
 *   agenda-setting beneficiary (organized/constrained) — fuses organizational
 *   identity with constitutional claims and forces questions onto the public
 *   agenda - ordinary_citizen_participants: diffuse beneficiaries
 *   (moderate/mobile) — gain standing to contest official meanings without
 *   credentials - political_minorities: dependent beneficiaries
 *   (powerless/constrained) — rely on appeal-over-institution channels when
 *   both courts and legislatures close - elected_representatives:
 *   dual-positioned (institutional/mobile) — gain interpretive voice, bear
 *   electoral and gridlock exposure - judicial_institutions: principal payers
 *   (institutional/identity_locked) — terminal authority withheld by design -
 *   certainty_dependent_commercial_interests: payers with partial exit
 *   (powerful/arbitrage) — absorb planning-cost uncertainty -
 *   long_horizon_public_programs: trapped payers (institutional/trapped) —
 *   mandates re-litigable mid-stream - disenfranchised_residents: excluded
 *   (powerless/trapped) — governed by meanings they cannot contest -
 *   future_generations: excluded non-acting party (powerless/trapped) —
 *   inherit deferred settlements - constitutional_theorists: analytical
 *   observers (analytical/analytical)
 *
 * KEY AGENTS:
 *   - grassroots_constitutional_movements: agenda-setting beneficiary (organized/constrained) — movements whose identities are fused with constitutional claims; they set which questions get contested
 *   - ordinary_citizen_participants: diffuse beneficiaries (moderate/mobile) — voters, jurors, petitioners gaining credential-free standing to contest
 *   - political_minorities: dependent beneficiaries (powerless/constrained) — groups whose recourse is appeal over institutional heads to wider publics
 *   - elected_representatives: dual-positioned beneficiary/payer (institutional/mobile) — branch-level interpretive voice purchased with electoral exposure
 *   - judicial_institutions: principal payers (institutional/identity_locked) — courts whose terminal authority is withheld by design and whose craft is discounted
 *   - certainty_dependent_commercial_interests: payers with partial exit (powerful/arbitrage) — firms absorbing planning-cost uncertainty, blunting exposure through private ordering
 *   - long_horizon_public_programs: trapped payers (institutional/trapped) — agencies whose multi-decade mandates rest on contestable interpretations
 *   - disenfranchised_residents: excluded (powerless/trapped) — governed by meanings produced over their heads
 *   - future_generations: excluded non-acting party (powerless/trapped) — inheritors of indefinitely deferred settlements
 *   - constitutional_theorists: analytical observers (analytical/analytical) — map the actual distribution of interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Distributed Interpretive Authority Without Terminal Adjudication").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'c01b443c-e563-47b9-abd8-cb46eccbb7bb').
narrative_ontology:cs_kernel_codification('c01b443c-e563-47b9-abd8-cb46eccbb7bb', fixed_text).
narrative_ontology:cs_authority_grounding('c01b443c-e563-47b9-abd8-cb46eccbb7bb', practice).
narrative_ontology:cs_interpretation_layer_present('c01b443c-e563-47b9-abd8-cb46eccbb7bb').
narrative_ontology:cs_reading_relation('c01b443c-e563-47b9-abd8-cb46eccbb7bb', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c01b443c-e563-47b9-abd8-cb46eccbb7bb', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('c01b443c-e563-47b9-abd8-cb46eccbb7bb', foundational, popular_interpretive_authorship).
narrative_ontology:cs_axiom_status(popular_interpretive_authorship, holdable).
narrative_ontology:cs_axiom_grounding('c01b443c-e563-47b9-abd8-cb46eccbb7bb', popular_interpretive_authorship, deontological).
narrative_ontology:cs_axiom('c01b443c-e563-47b9-abd8-cb46eccbb7bb', foundational, no_terminal_adjudication_of_meaning).
narrative_ontology:cs_axiom_status(no_terminal_adjudication_of_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c01b443c-e563-47b9-abd8-cb46eccbb7bb', no_terminal_adjudication_of_meaning, instrumental).
narrative_ontology:cs_reference_frame('c01b443c-e563-47b9-abd8-cb46eccbb7bb', popular_sovereignty_continuing_authorship).
narrative_ontology:cs_drift_state('c01b443c-e563-47b9-abd8-cb46eccbb7bb', contemporary_polarized_media_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c01b443c-e563-47b9-abd8-cb46eccbb7bb', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, grassroots_constitutional_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, ordinary_citizen_participants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, political_minorities).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, certainty_dependent_commercial_interests).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, long_horizon_public_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, departmentalism).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, republican_self_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civil-rights, labor, populist, and religious movements that identify constitutional provisions with their causes, mobilize members around interpretations, and force constitutional questions onto public agendas through protest, litigation support, and electoral pressure. Their influence depends on interpretive channels staying open; their organizational identities are fused with particular constitutional claims, so stepping back from contestation would mean dissolving the movements themselves.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, grassroots_constitutional_movements, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, grassroots_constitutional_movements, beneficiary).

% Voters, jurors, petitioners, and members of civic associations who take part in constitutional argument when issues become salient. They gain standing to contest official interpretations without credentials, and can withdraw from participation between episodes at little personal cost, resuming when a question touches their lives.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, ordinary_citizen_participants, beneficiary,
    moderate, biographical, mobile, national).

% Groups that lose in ordinary institutional arenas and rely on appeals over institutional heads — to wider publics, allied movements, or future electoral realignments. They cannot exit the polity that governs them, so open interpretive channels are their principal recourse when courts and legislatures both close against them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, political_minorities, beneficiary,
    powerless, biographical, constrained, national).

% Legislators and executives who claim independent interpretive authority for their branches and take constitutional positions during campaigns. They gain agenda power and insulation from judicial veto, but also bear electoral punishment for constitutional stances and absorb the policy instability that unsettled meanings create. Leaving office releases them from both the voice and the exposure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives, payer).

% Court systems whose rulings carry weight only so far as other institutions and publics continue to credit them. Final interpretive authority is withheld from them by design, and their specialized craft is discounted whenever it conflicts with mobilized public judgment. Yet the same openness replenishes their legitimacy when their rulings align with movement victories, and their professional identity is bound up with adjudication they cannot resign.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_institutions, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_institutions, beneficiary).

% Firms and investors whose contracting, valuation, and long-horizon investment presuppose stable constitutional rules on property, contract, and regulatory scope. Recurring re-contestation raises their planning costs; they respond with private ordering, contractual workarounds, jurisdiction selection, and lobbying for settlement, which blunts but does not eliminate their exposure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, certainty_dependent_commercial_interests, payer,
    powerful, biographical, arbitrage, continental).

% Government agencies administering multi-decade programs — infrastructure, social insurance, environmental regulation — whose statutory foundations rest on constitutional interpretations that later contestation can unsettle. Mandates can be re-litigated mid-stream; the agencies cannot abandon their missions while interpretations are in dispute.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, long_horizon_public_programs, payer,
    institutional, generational, trapped, national).

% Residents without full citizenship or voting rights who live under the constitution's commands and are shaped by its interpretations but have no standing in the contestation that produces them. They would object that meanings are being made over their heads; their objection registers nowhere in the arrangement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, disenfranchised_residents, excluded,
    powerless, biographical, trapped, national).

% People not yet born who will inherit whichever settlements recurrent contestation reaches or fails to reach. Perpetual openness defers closure indefinitely; they cannot participate now, yet every unsettled question compounds into their starting conditions. Kept on the roster for completeness; not a currently acting party.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, future_generations, excluded,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__popular_constitutionalism_reading, future_generations).

% Scholars in law and political theory who map how interpretive authority is actually distributed, compare this arrangement against rival designs in other democracies, and trace which claims about popular authorship survive contact with the historical record.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the constitutional interpretive activity of a large, diverse polity by keeping multiple sites — movements, elections, legislatures, courts, civic associations — simultaneously engaged, so that fundamental law tracks evolving public commitments and no single institution can freeze interpretation.
% TRANSFER_FUNCTION: Moves interpretive authority and agenda-setting power from credentialed institutions toward mobilized publics; moves interpretive certainty away from every actor, since no seat receives final answers; and transfers civic attention and organizing energy from private pursuits into recurring constitutional contestation.
% ABSENT_VOICES: Residents excluded from formal citizenship, future generations who inherit whatever settlements contestation reaches or fails to reach, and citizens without the leisure, literacy, or organizational access that sustained participation demands — the participatory burden falls unevenly, and those least able to contest are governed by meanings they had no hand in shaping. Legal specialists whose technical knowledge is discounted as anti-democratic are partially absent as well.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, some institution would rapidly consolidate terminal interpretive authority — historically, wherever popular constitutional vigilance lapsed, judicial or parliamentary supremacy filled the vacuum within a generation. The entire allocation of interpretive authority, movement strategy, litigation posture, and legislative constitutional practice would reorganize around whoever seized the last word.
% FOUNDING_PROBLEM: The recurring divergence between constitutional doctrine as administered by insulated institutions and the considered judgments of the governing public — the counter-majoritarian difficulty: how to keep fundamental law answerable to the people when the people's commitments evolve faster than institutional doctrine, without dissolving constitutional limits into momentary majoritarian preference.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: judicial-supremacy theorists who reject this reading nonetheless attest the underlying problem — the legitimacy crises surrounding Dred Scott, Lochner-era review, the New Deal confrontation, and Bush v. Gore are documented by legal historians across the interpretive spectrum; parliamentary-sovereignty advocates likewise concede the accountability deficit in insulated adjudication even while disputing this reading's remedy. The problem's persistence is attested by its opponents, which is the strongest available corroboration.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the arrangement transfers real value — terminal interpretive authority is withheld from courts, certainty is withheld from every seat, and gridlock costs land on long-horizon actors — but the transfer purchases a functioning coordination good, and no seat captures the proceeds; gains dissipate into widely held interpretive voice. Mid-range epsilon reflects that balance and sits well short of cover-story extraction because the coordination function is primary, verifiable, and repeatedly exercised. Suppression 0.42: enforcement is normative and structural rather than physical — court-curbing legislation, jurisdiction-stripping proposals, departmentalist defiance, civic refusal to treat judicial pronouncements as final. It suppresses terminal-authority claims, not persons; as a raw structural property it is reported unscaled, with scope and directionality scaling owned by the engine. Theater ratio 0.28: participation is substantially functional — movements have repeatedly moved constitutional meaning ahead of doctrine — but a growing share is performative (symbolic resolutions, social-media constitutionalism, ritualized hearings), tracked in the rising theater series. Accessibility collapse 0.30: understanding the arrangement does not close alternatives — rival interpretive designs remain live institutional possibilities, and within the arrangement every interpretive site stays open by design; low collapse is intrinsic to this reading. Resistance 0.55: courts reassert finality, bar associations defend expertise-gated interpretation, and settlement-seeking interests lobby against reopeners; resistance is persistent but not existential. Temporal series run on one shared eight-point grid (1960-2025) so every tracked metric is authored at every examined time point; all points are observed historical assessments. Extractiveness climbs as contestation polarizes and uncertainty costs compound; theater climbs with media-amplified performance; the suppression requirement climbs as judicial-prestige consolidation demands stronger countervailing mobilization to hold the no-terminal-authority line.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a different constraint than the beneficiary seats do. From the judiciary's position the arrangement reads as confiscation of a professionally earned trust and permanent institutional demotion; from certainty-dependent firms it reads as a standing tax on planning; from movements and political minorities it reads as the only mechanism that has ever actually worked — every major expansion of constitutional rights ran through extra-institutional contestation before doctrine caught up. The agenda-setter seat experiences the same structure as constitutive of democratic citizenship rather than as a cost imposed on it. Three institutional-power seats (courts, representatives, long-horizon programs) sit at identical nominal power with sharply different exits and exposures, which is precisely where per-seat computation earns its keep; the divergence between computed seats is the finding, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. Movements, citizen participants, and political minorities sit near the beneficiary pole: the arrangement subsidizes their voice, and for movements the identity fusion cuts exit, deepening engagement rather than exploitation. Courts sit near the full-target pole: they bear the withheld authority directly, and identity_locked exit amplifies their effective extraction because the institution cannot reposition away from adjudication. Long-horizon public programs are similarly target-side with trapped exit amplifying. Commercial interests are target-side but damped: arbitrage-grade exit (private ordering, contractual workaround, jurisdiction selection) pulls their effective extraction down from the raw victim position. Elected representatives are genuinely dual-positioned — voice gained, exposure borne — placing them near the middle; the derivation reads their beneficiary declaration while the secondary payer role and the gridlock-cost structure carry their exposure. No directionality_overrides were authored, deliberately: overrides key on power atoms, and this story's same-atom agents (three institutional seats) diverge by role and exit rather than by power, so role declarations carry the differentiation an override at this granularity could not express without misfiring across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping fundamental law answerable to an evolving public without dissolving constitutional limits into momentary preference — remains live, and is corroborated by the reading's opponents, which is the strongest available provenance. There is no sunset: the reading defines itself as permanent practice, not transition, so scaffold framing is unavailable. The tangled-rope classification guards against both mislabels: reading the arrangement as pure coordination ignores the real withheld-authority and uncertainty costs borne by named seats; reading it as cover-story extraction ignores that no seat captures the gains and that the coordination function is primary and demonstrably exercised. Holding both halves in one classification is exactly what the hybrid category exists to do. Mandatrophy is not resolved; the mismatch consumer should find status=live paired with verdict=world_rearranges, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the basic_law_interpretive_authority kernel; what changes structurally if a sibling reading governs instead?',
    'Adoption events: a jurisdiction shifting to the judicial-supremacy sibling converts distributed contestation into terminal adjudication — the victim set collapses to losers of specific cases, perpetual-contestability costs vanish, and this file''s epsilon no longer describes the operative arrangement; the parliamentary-sovereignty sibling concentrates termination electorally.',
    'Under either sibling, the arrangement this story describes ceases to be the standing arrangement; classification and epsilon must be re-authored for the successor arrangement rather than updated in place.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three mutually exclusive readings of whether any institution holds terminal interpretive authority.').

omega_variable(
    participatory_equality_vs_resource_capture,
    'Does open contestation actually distribute interpretive influence across the polity, or do resource-rich actors capture the channels?',
    'Comparative analysis of movement-driven versus funded-interest-driven doctrinal change across contestation episodes; campaign-finance and media-ownership data correlated with constitutional outcomes.',
    'If capture is systematic, the coordination function thins toward cover story and effective extraction on powerless beneficiaries rises, pushing the arrangement toward the extractive end despite diffusely declared gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participatory_equality_vs_resource_capture, empirical, 'Whether the participatory subsidy reaches the powerless or is captured by the resourced.').

omega_variable(
    contestability_feature_or_bug,
    'Is perpetual contestability an error-correction feature or a chronic-uncertainty defect?',
    'Not resolvable by data alone: it turns on the weight a polity places on responsiveness versus stability. Comparative constitutional-performance studies inform but do not settle the weighting.',
    'A polity weighting stability above responsiveness reads the same structure as heavily extractive; one weighting responsiveness above stability reads it as cheap coordination. Seat-level classifications diverge accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contestability_feature_or_bug, preference, 'Value-dependent evaluation of permanently unresolved interpretive openness.').

omega_variable(
    internalized_judicial_deference,
    'Is the observed shortfall in mass participation caused by structural barriers or by internalized deference — the learned belief that constitutional meaning belongs to lawyers and judges?',
    'Post-barrier participation trajectories: jurisdictions that lower participation costs (civic education, accessible constitutional briefing, movement infrastructure) reveal whether engagement follows; if it does not, the deficit is internalized rather than structural.',
    'If internalized, the arrangement''s suppression is understated by the structural measure — the deference travels with citizens even where channels stand open, and the beneficiary subsidy is weaker than the declarations suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_judicial_deference, empirical, 'Structural versus internalized restraint on popular interpretive participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1960, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1960, 0.14).
narrative_ontology:measurement_basis(basi_tr_t1960, observed).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement_basis(basi_tr_t1970, observed).
narrative_ontology:measurement(basi_tr_t1980, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(basi_tr_t1980, observed).
narrative_ontology:measurement(basi_tr_t1990, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement_basis(basi_tr_t1990, observed).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(basi_tr_t2000, observed).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement_basis(basi_tr_t2010, observed).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(basi_tr_t2020, observed).
narrative_ontology:measurement(basi_tr_t2025, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(basi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t1960, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1960, 0.36).
narrative_ontology:measurement_basis(basi_be_t1960, observed).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1970, 0.34).
narrative_ontology:measurement_basis(basi_be_t1970, observed).
narrative_ontology:measurement(basi_be_t1980, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1980, 0.37).
narrative_ontology:measurement_basis(basi_be_t1980, observed).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1990, 0.41).
narrative_ontology:measurement_basis(basi_be_t1990, observed).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(basi_be_t2000, observed).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement_basis(basi_be_t2010, observed).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement_basis(basi_be_t2020, observed).
narrative_ontology:measurement(basi_be_t2025, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(basi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1960, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement_basis(basi_su_t1960, observed).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1970, 0.32).
narrative_ontology:measurement_basis(basi_su_t1970, observed).
narrative_ontology:measurement(basi_su_t1980, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1980, 0.33).
narrative_ontology:measurement_basis(basi_su_t1980, observed).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement_basis(basi_su_t1990, observed).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2000, 0.37).
narrative_ontology:measurement_basis(basi_su_t2000, observed).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement_basis(basi_su_t2010, observed).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement_basis(basi_su_t2020, observed).
narrative_ontology:measurement(basi_su_t2025, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(basi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the constitution' decomposes into three structurally distinct arrangements with different beneficiary/victim sets and different epsilon values. This file is the popular-constitutionalist member of the family; its epsilon refers only to the distributed-contestation arrangement. Family links run through network.affects_constraints to both siblings; the upstream/downstream citation traffic between readings (each side citing the others' failures as evidence) is documented in the kernel_reading_position omega rather than in metric adjustments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
