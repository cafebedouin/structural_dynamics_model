% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality-Clause Scope: Progressive Textualist Reading (Amendment-Gated Expansion)
 *   domain: political/constitutional/legal
 *
 * SUMMARY:
 *   A written constitution declares an equality principle; under this
 *   reading, the principle's application scope is part of the enacted text
 *   and widens only when a supermajority of the states consents through the
 *   amendment procedure — never by judicial elaboration of the principle's
 *   spirit. The arrangement under assessment is a two-part machine: a
 *   high-consent channel that delivered the Reconstruction and suffrage
 *   expansions, and a judicial-restraint discipline that keeps the
 *   interpretive shortcut closed. This story instantiates ONE reading of the
 *   contested kernel equality_clause_scope — the progressive_textualist
 *   reading — and authors epsilon for the standing amendment-governed
 *   arrangement as this reading sees it: a real coordination achievement
 *   carrying real interim costs. The sibling readings
 *   (restrictive_originalist, expansive_universalist) are separate
 *   constraints in separate files with their own epsilon values, beneficiary
 *   sets, and classifications; they are linked through the network surface,
 *   not folded in here. The claim/metric relationship is deliberately
 *   unreconciled: the reading CLAIMS tangled_rope (genuine coordination plus
 *   asymmetric burden) while the metrics are authored from the arrangement's
 *   observable operation — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - - federal_amendment_institutions: Agenda setter (institutional/constrained) — holds the proposal monopoly over every scope question
 *   - - state_ratifying_legislatures: Beneficiary (institutional/constrained) — holds an absolute ratification veto over constitutional meaning
 *   - - supermajority_insulated_regional_interests: Beneficiary (powerful/arbitrage) — purchases insulation from narrow-majority change
 *   - - restraint_doctrine_judiciary: Enforcing agenda setter (institutional/identity_locked) — administers interpretive self-limitation
 *   - - deferred_equality_claimants: Primary payer (powerless/trapped) — bears interim exclusion until a supermajority forms
 *   - - civil_rights_movement_coalitions: Payer/beneficiary (organized/constrained) — pays campaign costs, collects the durability premium
 *   - - expansive_interpretation_advocates: Payer (moderate/identity_locked) — preferred channel ruled out of order
 *   - - territorial_residents: Excluded (powerless/trapped) — governed by a process in which they hold no franchise
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — sees the full structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.61).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.68).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.61).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality-Clause Scope: Progressive Textualist Reading (Amendment-Gated Expansion)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "political/constitutional/legal").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, federal_amendment_institutions).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, state_ratifying_legislatures).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, supermajority_insulated_regional_interests).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, deferred_equality_claimants).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, expansive_interpretation_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, civil_rights_movement_coalitions).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, civil_rights_movement_coalitions).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, article_v_amendment_supremacy).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, bounded_judicial_review_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress proposes constitutional amendments by two-thirds vote of both houses and thereby controls which proposed expansions of the equality guarantee ever reach the states for ratification. No expansion passes without its agenda consent, and it holds the agenda-setting position over every question of constitutional scope. Its exit from the arrangement would require dismantling the very procedure it administers.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, federal_amendment_institutions, agenda_setter,
    institutional, generational, constrained, national).

% State legislative bodies hold an absolute veto over any amendment: three-fourths of the states must ratify before an expansion takes effect. This gives each state's legislature a permanent gatekeeping seat over the meaning of the equality guarantee regardless of national majorities or court composition. They collect ratification authority continuously; their consent is the scarce input every expansion needs.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, state_ratifying_legislatures, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, state_ratifying_legislatures, agenda_setter).

% Regional political communities whose local arrangements would not survive a narrow national majority or a sympathetic court benefit from the high consent threshold: nothing about their status can change unless a supermajority spanning most of the country agrees. They purchase insulation without administering anything, and they can shift political activity among jurisdictions if any single forum turns hostile.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, supermajority_insulated_regional_interests, beneficiary,
    powerful, generational, arbitrage, regional).

% Federal judges committed to reading the equality guarantee according to its enacted text and original public meaning. They decline to widen the guarantee's reach by interpretation, referring scope questions to the amendment process, and they police that discipline in their own opinions and in the profession's norms. Their career standing and intellectual identity are bound up with the self-limitation; abandoning it would mean repudiating the jurisprudence that defines them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, restraint_doctrine_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% People whose equal standing under the written guarantee awaits a supermajority that has not yet formed. They cannot leave the polity that governs them except at prohibitive personal cost, and they bear the full weight of the wait: exclusion in schooling, voting, marriage, jury service, or legal personhood continues until enough states consent. Historically the wait has run generations, outlasting the lifetimes of those who first pressed the claim.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, deferred_equality_claimants, payer,
    powerless, biographical, trapped, national).

% Organized movements that campaign for expansion through the amendment channel itself — petitioning, lobbying state legislatures, building the supermajorities the procedure demands. They absorb the cost of decades-long campaigns and repeated defeats, and they are also the parties best positioned to collect the durability premium when an amendment finally passes, since rights won this way arrive with broad consent attached.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, civil_rights_movement_coalitions, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, civil_rights_movement_coalitions, beneficiary).

% Jurists, scholars, and litigants who argue the guarantee's reach should widen through judicial interpretation of its principle rather than amendment. Within this arrangement their preferred channel is treated as illegitimate, so their arguments register as minority dissents rather than operative law. Their professional identities are fused with the interpretive approach; abandoning it would cost them their standing in their own tradition.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, expansive_interpretation_advocates, payer,
    moderate, biographical, identity_locked, national).

% Residents of the District of Columbia and the territories live under the written guarantee and its amendment procedure but cannot vote for the bodies that propose or ratify amendments. When the scope of the guarantee is renegotiated, they are governed by a process in which they hold no franchise — their status is decided entirely by others.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, territorial_residents, excluded,
    powerless, biographical, trapped, regional).

% Academic observers who compare amendment difficulty and rights durability across national constitutions, code the success rates of proposed amendments, and publish on whether supermajority channels still function. They take no side in the dispute and bear none of its costs; their seat is analytic.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, federal_amendment_institutions).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the constitutional-change collective-action problem: provides a single durable channel through which the equality principle's scope can widen with broad consent, preventing both frozen meaning and judge-by-judge volatility in constitutional commitments.
% TRANSFER_FUNCTION: Moves decision-making authority over the equality guarantee's meaning from courts to supermajority amendment coalitions; moves the costs of interim exclusion onto those whose claims await consensus; attaches a durability and legitimacy premium to rights won by amendment relative to rights won by decree.
% ABSENT_VOICES: Territorial residents, noncitizen residents, and the disenfranchised classes whose status the channel decides would object that the procedure governing their equal standing excludes their voice at precisely the moments their status is set — they stand outside the ratification franchise, sometimes outside the polity, by design rather than by accident.
% DISAPPEARANCE_RATIONALE: If the amendment-only rule vanished overnight, authority over the equality guarantee's meaning would migrate immediately to whichever institution moved first — courts through interpretive takeover or congressional majorities through ordinary legislation. The durability premium attached to amendment-won rights would evaporate, insulated regional interests would lose their shield against narrow majorities, and the distribution of constitutional authority would rearrange within a single litigation cycle.
% FOUNDING_PROBLEM: How can a written constitution containing an equality principle bind across generations without either freezing its eighteenth-century exclusions in place or surrendering its meaning to transient interpretive majorities — that is, how to make constitutional change both possible and legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative constitutional scholars in the amendment-difficulty literature attest the stability-versus-revision tension persists across systems; historians of the Reconstruction Amendments attest the founding problem drove the era's design choices; dissenting jurists who reject this reading's conclusions nonetheless concede Article V's legitimacy in their own opinions. No attestation relies on the benefiting parties alone.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.61 at interval end: the channel has repeatedly delivered (13th-15th, 19th, 24th, 26th Amendments), but between firings the entire burden of the principle's unfinished scope falls on those with the least voice, and the current dormancy phase (no rights-expanding amendment since 1971) has let the accumulated burden climb for five decades. Suppression is 0.68 and predominantly structural rather than internalized: Article V's formal supermajority barriers plus the precedent-and-profession machinery that maintains judicial restraint; a smaller internalized component lives in the judiciary's professional self-conception. Theater_ratio 0.46 and rising: twenty-seven amendments in 235 years, none since 1992, none expanding individual rights since 1971 — the channel is ceremonially central and practically near-dormant, so a growing share of activity around it is commemorative rather than operational. Accessibility_collapse 0.42: alternatives (statutory civil rights, state constitutional guarantees, occasional judicial departures) remain partly open, which is why the arrangement must be actively defended. Resistance 0.58: sustained litigation, living-constitution scholarship, and movement pressure meet the restraint discipline continuously. The measurement series share one nine-point grid (t=0..230, roughly 1791-2021); extractiveness traces a pulse-driven sawtooth — each amendment firing drops it sharply, each dormancy phase lets it accumulate — and the oscillation is a side effect of the channel's episodic structure rather than an engineered reinforcement schedule, though the dormancy phases function as accumulation mechanisms. Base_properties report the interval-end state (t=230, late dormancy), i.e., values on the rising limb of the current accumulation phase. Receipt: the arrangement's gains accrue most demonstrably to the federal amendment institutions, whose proposal monopoly converts every deferred claim into agenda control — hence gain_flow names that seat; ratifying legislatures and insulated interests collect adjacent slices, so the receipt is concentrated rather than diffuse. Fixing the arrangement runs through Article V itself — the supermajority that would have to consent to loosening the supermajority requirement is the same barrier being removed — so the cost class is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   From the deferred_equality_claimants seat the arrangement computes as heavy extraction with distant relief: the threshold that looks like prudence from the agenda-setter seats looks like indefinite deferral from inside a truncated life. From the amendment-institution seats the same structure is the price of rights that arrive with consent attached and therefore stay won. The judiciary seat is the sharpest divergence: it administers a discipline that forbids it the very power the sibling readings would hand it — an identity-fused self-limitation in which professional identity (fidelity to enacted text as the judge's defining virtue) does the work that external coercion does elsewhere. If that identity frame broke — if the textualist coalition dissolved and judges reclaimed interpretive scope — the arrangement's enforcement pillar would fail without any formal rule changing, and authority over equality's meaning would migrate to whichever institution seized the vacated ground. The expansive_interpretation_advocates seat mirrors the lock ideologically: exit from living-constitutionalism is unthinkable within their jurisprudential self-concept, so they persist as a loyal opposition whose dissent stabilizes the very threshold they oppose.
 *
 * DIRECTIONALITY LOGIC:
 *   Amendment institutions, ratifying legislatures, and insulated regional interests are declared beneficiaries and derive low directionality — the arrangement subsidizes them (agenda control, veto power, insulation), so effective extraction damps toward zero or inverts. Deferred equality claimants are declared victims with trapped exit and derive near-full-target directionality — their effective extraction is amplified, and national spatial scope adds the engine's verification-difficulty modifier. Movement coalitions straddle the line: declared payers who also collect the durability premium, their derived directionality lands mid-range rather than at the target pole. The restraint-doctrine judiciary is the case the derivation chain handles worst: an agenda_setter that collects prestige but forgoes interpretive power sits slightly target-side of symmetric, and a role-derived reading would misplace it among beneficiaries. A power-atom-keyed override cannot separate it from the other institutional seats, so the correction is documented here rather than forced through the override surface. Suppression enters the engine raw and unscaled; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a written equality principle can bind across generations without either freezing its eighteenth-century exclusions or surrendering its meaning to transient interpretive majorities — is still live, and the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no zombie flag. Mandatrophy discipline blocks the two symmetrical misreadings. Reading the arrangement as a pure snare erases the genuine coordination achievement: amendment-won expansions (Reconstruction, suffrage) proved durable in exactly the way court-decreed expansions historically have not, which is this reading's strongest evidence. Reading it as a pure rope erases the asymmetry: the coordination benefit arrives on supermajority time while its costs are paid on biographical time by people who did not consent to the wait. The arrangement is mid-life, not atrophied — the channel still fires occasionally and the restraint discipline still binds — so mandatrophy is not resolved and no sunset machinery applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_split,
    'This story instantiates the progressive_textualist reading of kernel equality_clause_scope; do the structural commitments authored here (bounded universalism, supermajority-gated expansion) represent the kernel faithfully, or would the sibling readings'' commitments produce a different constraint with a different victim set and epsilon from the same text?',
    'Cross-reading comparison: compile all three sibling stories and compare computed per-seat classifications on shared scenario perturbations; divergent victim sets and epsilon ranges locate the commitment split.',
    'Adopting the expansive_universalist commitment removes the legitimating threshold — every historical exclusion becomes uncompensated delay and epsilon rises toward the snare range; adopting the restrictive_originalist commitment inverts the beneficiary and victim sets entirely, making most of the population the extracted class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_split, conceptual, 'Committer split: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    disagreement_location_authority_allocation,
    'The readings agree that equality binds; the disagreement is located in who may enlarge its scope — courts by interpretation or the states by supermajority amendment. Which allocation of enlargement authority does the structural evidence favor?',
    'Institutional-performance analysis comparing the durability and backlash records of court-decreed versus amendment-won expansions across jurisdictions and eras.',
    'If amendment-won expansions systematically outlast court decrees, this reading''s threshold is vindicated as coordination; if outcomes are equivalent, the threshold is exposed as a delay mechanism and the arrangement slides toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_authority_allocation, conceptual, 'Where the kernel contest actually bites: allocation of enlargement authority.').

omega_variable(
    amendment_channel_accessibility,
    'Is the Article V channel still practically accessible, or has it de facto collapsed — twenty-seven amendments in 235 years, none since 1992, none expanding individual rights since 1971, the Equal Rights Amendment falling short of ratification?',
    'Time-series analysis of amendment proposal, passage, and ratification rates; coalition-size modeling of what a rights expansion now requires against current polarization levels.',
    'If the channel is effectively dead, the coordination claim is vestigial: the arrangement drifts toward piton (ceremonial threshold) or snare (threshold as pure blockade), and the rising theater_ratio series already trends consistent with partial collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_accessibility, empirical, 'Whether the legitimating channel still functions or has gone ceremonial.').

omega_variable(
    interim_exclusion_cost_accounting,
    'How large is the welfare cost borne by deferred equality claimants during amendment lags, relative to the stability and legitimacy benefits the supermajority threshold purchases?',
    'Historical counterfactual analysis of episodes where judicial expansion preceded consensus, comparing the durability and backlash of those gains against amendment-won equivalents.',
    'If interim costs dominate, the measured extraction is asymmetric rent and the arrangement classifies closer to snare; if stability benefits dominate, the extraction is largely coordination cost and the arrangement sits closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interim_exclusion_cost_accounting, empirical, 'Cost accounting of the wait imposed on equality claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__progressive_textualist, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__progressive_textualist, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(equa_tr_t60, observed).
narrative_ontology:measurement(equa_tr_t75, equality_clause_scope__progressive_textualist, theater_ratio, 75, 0.18).
narrative_ontology:measurement_basis(equa_tr_t75, observed).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__progressive_textualist, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(equa_tr_t120, observed).
narrative_ontology:measurement(equa_tr_t150, equality_clause_scope__progressive_textualist, theater_ratio, 150, 0.32).
narrative_ontology:measurement_basis(equa_tr_t150, observed).
narrative_ontology:measurement(equa_tr_t180, equality_clause_scope__progressive_textualist, theater_ratio, 180, 0.3).
narrative_ontology:measurement_basis(equa_tr_t180, observed).
narrative_ontology:measurement(equa_tr_t210, equality_clause_scope__progressive_textualist, theater_ratio, 210, 0.41).
narrative_ontology:measurement_basis(equa_tr_t210, observed).
narrative_ontology:measurement(equa_tr_t230, equality_clause_scope__progressive_textualist, theater_ratio, 230, 0.46).
narrative_ontology:measurement_basis(equa_tr_t230, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__progressive_textualist, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__progressive_textualist, base_extractiveness, 60, 0.72).
narrative_ontology:measurement_basis(equa_be_t60, observed).
narrative_ontology:measurement(equa_be_t75, equality_clause_scope__progressive_textualist, base_extractiveness, 75, 0.46).
narrative_ontology:measurement_basis(equa_be_t75, observed).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__progressive_textualist, base_extractiveness, 120, 0.58).
narrative_ontology:measurement_basis(equa_be_t120, observed).
narrative_ontology:measurement(equa_be_t150, equality_clause_scope__progressive_textualist, base_extractiveness, 150, 0.54).
narrative_ontology:measurement_basis(equa_be_t150, observed).
narrative_ontology:measurement(equa_be_t180, equality_clause_scope__progressive_textualist, base_extractiveness, 180, 0.44).
narrative_ontology:measurement_basis(equa_be_t180, observed).
narrative_ontology:measurement(equa_be_t210, equality_clause_scope__progressive_textualist, base_extractiveness, 210, 0.57).
narrative_ontology:measurement_basis(equa_be_t210, observed).
narrative_ontology:measurement(equa_be_t230, equality_clause_scope__progressive_textualist, base_extractiveness, 230, 0.61).
narrative_ontology:measurement_basis(equa_be_t230, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__progressive_textualist, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__progressive_textualist, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(equa_su_t60, observed).
narrative_ontology:measurement(equa_su_t75, equality_clause_scope__progressive_textualist, suppression_requirement, 75, 0.42).
narrative_ontology:measurement_basis(equa_su_t75, observed).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__progressive_textualist, suppression_requirement, 120, 0.55).
narrative_ontology:measurement_basis(equa_su_t120, observed).
narrative_ontology:measurement(equa_su_t150, equality_clause_scope__progressive_textualist, suppression_requirement, 150, 0.58).
narrative_ontology:measurement_basis(equa_su_t150, observed).
narrative_ontology:measurement(equa_su_t180, equality_clause_scope__progressive_textualist, suppression_requirement, 180, 0.62).
narrative_ontology:measurement_basis(equa_su_t180, observed).
narrative_ontology:measurement(equa_su_t210, equality_clause_scope__progressive_textualist, suppression_requirement, 210, 0.66).
narrative_ontology:measurement_basis(equa_su_t210, observed).
narrative_ontology:measurement(equa_su_t230, equality_clause_scope__progressive_textualist, suppression_requirement, 230, 0.68).
narrative_ontology:measurement_basis(equa_su_t230, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% The colloquial label 'the constitutional equality guarantee' decomposes into three structurally distinct constraints sharing one kernel (equality_clause_scope). The restrictive_originalist reading authors the highest epsilon: permanent exclusion of everyone outside the eighteenth-century franchise, with nearly the whole population in the victim set. This progressive_textualist reading authors moderate epsilon for the standing arrangement: real interim exclusion pending supermajority, offset by a genuine coordination function. The expansive_universalist reading authors the standing arrangement as highly extractive (every historical exclusion is unjustified delay) while endorsing a near-zero-extraction alternative. Family topology: restrictive_originalist is upstream (the historical baseline both other readings argue from and against); this reading mediates between it and the downstream-contested expansive_universalist. Each member links to the others via network.affects_constraints; no member folds another's contest into its own epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
