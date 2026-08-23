% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Hybrid Reading of the Quran-Hadith Substrate (Sovereign Selection of Scriptural Law)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   Across the post-caliphate states, a characteristic legal architecture
 *   emerged: classical fiqh rulings are adopted selectively into family and
 *   personal-status codes and displayed in criminal provisions, while
 *   commerce, finance, and administration run on secular or reformist
 *   frameworks. Legitimacy for this arrangement rests on political
 *   sovereignty - the state, not scholarly consensus and not ethical
 *   reinterpretation, decides which scriptural rulings bind where. This file
 *   instantiates ONE reading of the quran_hadith_substrate kernel: the
 *   state_hybrid reading. Per the epsilon-invariance principle, the sibling
 *   readings (traditionalist_taqlid, reformist_ijtihad) are separate
 *   constraints in separate files; neither is folded into this story's
 *   epsilon, beneficiaries, or verdicts. The epsilon referent is the standing
 *   arrangement under contest - the state's selective-adoption regime as it
 *   actually operates - assessed by this reading's own lights, never the
 *   comprehensive-sharia or fully-reformist arrangements the flanks would
 *   install. KEY AGENTS (by structural relationship): governing_state_elites:
 *   agenda-setting sovereign (institutional/arbitrage) - owns the selection
 *   boundary and collects the legitimacy returns;
 *   state_religious_bureaucracy: administered interpreter
 *   (institutional/constrained) - produces official rulings, position
 *   conditional on state service; traditionalist_scholars: truncated
 *   authority flank (organized/constrained) - madhhab consensus reduced to
 *   state-selected fragments; reformist_intellectuals: censored critic flank
 *   (moderate/trapped) - toolkit borrowed, voice prosecuted;
 *   ordinary_citizens: dual-positioned subjects (moderate/constrained) -
 *   receive the legal order, bear its selected penalties;
 *   commercial_financial_elites: sheltered beneficiary (powerful/arbitrage) -
 *   secular commercial rails, mobile capital;
 *   islamist_comprehensive_sharia_movements: excluded comprehensive claimant
 *   (organized/trapped) - barred from the codification conversation;
 *   international_rights_monitors: analytical observer
 *   (institutional/analytical) - reviews, recommends, cannot vote.
 *
 * KEY AGENTS:
 *   - governing_state_elites: agenda-setting sovereign (institutional/arbitrage) - owns the selection boundary between classical and secular domains and collects the arrangement's legitimacy returns
 *   - state_religious_bureaucracy: administered interpreter (institutional/constrained) - official muftiates and codification boards whose gains are conditional on political service
 *   - traditionalist_scholars: truncated authority flank (organized/constrained) - madhhab jurists whose binding-consensus claim survives only in state-chosen fragments
 *   - reformist_intellectuals: censored critic flank (moderate/trapped) - contextual interpreters whose methods are appropriated in commerce and prosecuted in family/criminal critique
 *   - ordinary_citizens: dual-positioned subjects (moderate/constrained) - receive a working legal order and identity continuity, bear selected classical penalties and bureaucratic costs
 *   - commercial_financial_elites: sheltered beneficiary (powerful/arbitrage) - operate interest-based finance and flexible contracting under the secular rails the hybrid preserves
 *   - islamist_comprehensive_sharia_movements: excluded comprehensive claimant (organized/trapped) - demand unpartitioned application, banned from the codification process
 *   - international_rights_monitors: analytical observer (institutional/analytical) - treaty bodies and NGOs reviewing family-code and penal provisions without a codification vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.4).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.62).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Hybrid Reading of the Quran-Hadith Substrate (Sovereign Selection of Scriptural Law)").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "religious/legal/political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '98de2289-36ff-45b1-afd7-620c7515b3bf').
narrative_ontology:cs_kernel_codification('98de2289-36ff-45b1-afd7-620c7515b3bf', fixed_text).
narrative_ontology:cs_authority_grounding('98de2289-36ff-45b1-afd7-620c7515b3bf', extraction).
narrative_ontology:cs_interpretation_layer_present('98de2289-36ff-45b1-afd7-620c7515b3bf').
narrative_ontology:cs_reading_relation('98de2289-36ff-45b1-afd7-620c7515b3bf', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('98de2289-36ff-45b1-afd7-620c7515b3bf', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('98de2289-36ff-45b1-afd7-620c7515b3bf', foundational, political_sovereignty_over_doctrinal_fidelity).
narrative_ontology:cs_axiom_status(political_sovereignty_over_doctrinal_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('98de2289-36ff-45b1-afd7-620c7515b3bf', political_sovereignty_over_doctrinal_fidelity, conventional).
narrative_ontology:cs_axiom('98de2289-36ff-45b1-afd7-620c7515b3bf', foundational, domain_partitioned_revelation_application).
narrative_ontology:cs_axiom_status(domain_partitioned_revelation_application, holdable).
narrative_ontology:cs_axiom_grounding('98de2289-36ff-45b1-afd7-620c7515b3bf', domain_partitioned_revelation_application, instrumental).
narrative_ontology:cs_reference_frame('98de2289-36ff-45b1-afd7-620c7515b3bf', sovereign_administered_sharia).
narrative_ontology:cs_drift_state('98de2289-36ff-45b1-afd7-620c7515b3bf', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98de2289-36ff-45b1-afd7-620c7515b3bf', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, governing_state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_religious_bureaucracy).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_financial_elites).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_intellectuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, ordinary_citizens).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, ordinary_citizens).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, political_sovereignty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, maslaha_public_interest_legislation).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, domain_partitioned_sharia_codification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold executive and legislative power in a state whose constitution names Islam as a source or the source of legislation. Decide which classical rulings enter the family and criminal codes and which domains run on secular or reformist frameworks. Collect the arrangement's central returns: religious legitimacy among pious constituencies, leverage over dissenting scholars, and an unencumbered commercial-administrative apparatus. They wrote the selection rules and can redraw them; their exit is effectively unlimited.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, governing_state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Official muftiates, ministries of religious affairs, and codification boards staffed by state-appointed scholars. Produce the interpretations the state adopts, administer religious courts and endowments, and draw salaries, rank, and patronage for doing so. Their positions exist at the state's pleasure; when the selection boundary shifts, their rulings must follow. Leaving means forfeiting the institutional platform that makes their scholarship audible.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_religious_bureaucracy, beneficiary,
    institutional, biographical, constrained, national).

% Madhhab-trained jurists and seminary networks outside the state payroll. Hold that the classical schools carry binding consensus and watch the state adopt fragments of that consensus while subordinating the whole to political convenience. Teaching licenses, endowment income, and publishing channels depend on state approval; full exit means preaching from informal circles with attenuated reach, or relocating to jurisdictions where their credentials travel better.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, regional).

% Academics, lawyers, and preachers arguing for contextual reinterpretation - prioritizing the Quran's ethical trajectory and public interest over literal hadith application. The state borrows their toolkit where convenient, citing public interest to justify commercial reform, while censoring, defrocking, or jailing them when their critiques touch the family code or the criminal provisions displayed for legitimacy. Publishing abroad or online carries exile or prosecution risk; falling silent ends their vocation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_intellectuals, payer,
    moderate, biographical, trapped, national).

% Live under the partitioned system: marriages, divorces, and inheritances adjudicated by codified classical provisions; contracts, employment, and administration governed by secular frameworks. Receive a working legal order and continuity of religious identity; bear the costs where selected classical provisions bite - unequal divorce rights, corporal or capital penalties displayed but unevenly applied - and fund the religious bureaucracy through taxation. Emigration is possible for a skilled minority but severs family and community for most.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, ordinary_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, ordinary_citizens, payer).

% Bankers, industrialists, and professionals operating under the secular commercial frameworks the hybrid preserves. Run interest-based finance, flexible contracting, and predictable regulation that comprehensive classical application would forbid, while the state's religious credentials stabilize the social order they trade in. Capital is mobile; adverse shifts in the selection boundary can be hedged by relocation or offshore structures.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_financial_elites, beneficiary,
    powerful, biographical, arbitrage, global).

% Movements demanding the classical corpus govern all domains without state selectivity. Barred from the codification process, banned or restricted as parties, their leaders imprisoned or exiled. They contest the arrangement's premise from outside a conversation they are legally excluded from; their options are prison, underground organization, or exile.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, islamist_comprehensive_sharia_movements, excluded,
    organized, generational, trapped, national).

% Treaty bodies, special rapporteurs, and NGOs reviewing the family code's gender provisions and the criminal code's corporal penalties. Take testimony, issue findings, and recommend reform; hold no vote in codification and no enforcement lever beyond diplomatic pressure. See the full structure from outside it.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_rights_monitors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, governing_state_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-colonial dual-inheritance problem: one interoperable legal order spanning religiously-coded personal status law and modern commercial-administrative regulation, so courts, registries, and ministries function together while the state sustains a credible Islamic identity claim.
% TRANSFER_FUNCTION: Moves interpretive authority over the Quran-hadith substrate from independent scholarly institutions to state organs; moves legitimacy returns to ruling elites and stipends, rank, and patronage to the official clergy; moves the enforcement burden of selected classical provisions onto citizens subject to the family and criminal codes.
% ABSENT_VOICES: Comprehensive-sharia movements are banned from the codification conversation; independent madhhab jurists outside the state payroll hold no seat; reformist critics are heard only where their conclusions suit the state; women subject to the family code's classical provisions were rarely seated in the codifying assemblies. They are in prisons, exile, informal study circles, and foreign faculties.
% DISAPPEARANCE_RATIONALE: If the selection boundary vanished overnight, every hybrid state would face the choice its founders deferred: comprehensive classical application (collapsing interest-based finance and modern administration) or full secularization (stripping the regime's religious legitimacy claim). Religious bureaucracies would dissolve or defect, commercial law would need refounding on one basis or the other, and the legitimacy contest between palace, pulpit, and street would reopen at full intensity.
% FOUNDING_PROBLEM: After the caliphate's abolition and decolonization, new states inherited European-drafted commercial and administrative codes while needing internal legitimacy among religious populations: how to be both a modern governing state and credibly Islamic without surrendering either.
% FOUNDING_PROBLEM_CORROBORATION: Historians of nineteenth- and twentieth-century legal transplantation (Ottoman Mecelle codification, Egyptian mixed courts, colonial-era fiqh codification projects) attest the founding problem from outside the benefiting parties: post-colonial states did inherit European commercial codes and did face a religious legitimacy deficit. Opposition ulama and human-rights litigants corroborate that the problem's original terms have shifted and dispute whether the current arrangement still answers it; no attestation of continued problem-fit rests on the beneficiary set alone.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. Claimed type tangled_rope rests on structure alone: the arrangement solves a real coordination problem (one interoperable legal order spanning religiously-coded personal status law and modern commercial administration) AND carries asymmetric extraction through the same structure (legitimacy rents to the sovereign, truncation of scholarly authority, censorship of critics), AND requires continuous active enforcement (without policing, traditionalists would extend fiqh into commerce and reformists would extend critique into the family code - the boundary exists only because it is patrolled). Metrics are authored descriptively: extractiveness 0.40 sits mid-bin (real coordination delivered, real rents collected, both scholarly flanks taxed); suppression 0.62 is a raw structural property, deliberately NOT scaled by power or scope - only extractiveness is scaled downstream; theater_ratio 0.48 reflects a growing share of sharia activity that is symbolic display (codified-but-rarely-applied penalties, pageant piety, official iftar politics) approaching the Goodhart threshold; accessibility_collapse 0.50 records that off-channel alternatives persist (private study circles, foreign presses, diaspora fatwa platforms) while official-channel alternatives have collapsed; resistance 0.60 records sustained two-flank resistance. The measurement series run on ONE shared nine-point grid (1924-2024) so every metric is authored at every examined time point. The suppression series OSCILLATES (crackdown-accommodation cycles tracking regime insecurity, oil-boom patronage waves, and Islamic-resurgence shocks) while ratcheting upward overall; the oscillation is partly the mechanism itself - intermittent enforcement keeps both flanks uncertain and dependent more cheaply than constant repression - and partly exogenous shock response. Base_properties scalars reflect the interval ENDPOINT (2024), a high-suppression phase of the cycle; mid-cycle readings would show lower suppression.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the governing elite seat the arrangement is masterful statecraft - a rope-like solution to the modernity-legitimacy dilemma the founders inherited. From the traditionalist seat the same structure is a seizure operation: politicians administering God's law by convenience, madhhab consensus demoted to a menu. From the reformist seat it is selective authoritarianism - the state borrows ijtihad's tools where flexibility pays and jails ijtihad's practitioners where critique threatens the family code's legitimacy yield. From the citizen seat it is mostly background order with occasional sharp edges (divorce asymmetry, displayed penalties). The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. governing_state_elites sit near the beneficiary pole (d near 0): they wrote the selection rules, collect the legitimacy returns, and hold arbitrage-grade exit. commercial_financial_elites likewise derive low d - the secular commercial rails subsidize them and their capital is mobile. state_religious_bureaucracy derives low d from its beneficiary declaration, but the derivation understates its exposure: its gains are conditional on political service, and when the selection boundary shifts its rulings must follow - it is an instrument, not a principal. ordinary_citizens are dual-declared in the stakeholder surface (beneficiary with secondary payer role) and sit near symmetric d: genuine coordination benefit, diffuse indirect cost. traditionalist_scholars and reformist_intellectuals sit near the target pole (d near 1) with differentiated exit: traditionalists are constrained (institutional entanglement - licenses, endowments, audibility - rather than cages), reformists are trapped where censorship and prosecution bite. No directionality_overrides are authored: overrides key on power atoms, and this story's atoms collide (institutional covers both elites and bureaucracy; moderate covers both reformists and citizens), so any override would drag innocent seats. The excluded Islamist seat and the analytical observer seat sit outside the chi derivation by design - authored absence is commentary-grade, never correction-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - reconciling inherited European commercial-administrative codes with internal religious legitimacy after the caliphate's fall - is genuinely contested rather than dead: the legitimacy-plus-modernity dilemma persists wherever these states exist, but the arrangement's center of gravity has shifted from problem-solving toward regime maintenance. Evidence for the shift is in the data, not the narrative: theater_ratio climbed 0.18 to 0.48 across the interval while extractiveness rose monotonically - the classic signature of a coordination shell accumulating rent. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-problem zombie flag is asserted, but if the legitimacy yield collapses fully (omega legitimacy_yield_trajectory), the arrangement completes the drift toward piton - maintained by inertia and performance, costly to fix, profiting no one substantively. Mandatrophy is therefore unresolved and honestly marked as contested rather than declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the quran_hadith_substrate kernel (reading: state_hybrid). Would instantiating a sibling reading - traditionalist_taqlid or reformist_ijtihad - change the constraint''s beneficiary/victim structure and classification?',
    'Author and compile the sibling stories as separate files; compare computed seat classifications across the kernel family. Under traditionalist_taqlid the state''s selective adoption becomes unauthorized seizure of scholarly authority and the payer set expands; under reformist_ijtihad the state''s retention of classical family and criminal provisions becomes the primary extraction object.',
    'Each reading produces a different victim set and a different epsilon referent; cross-reading comparison is valid only at the kernel level, never by averaging readings into one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which reading of the substrate organizes law determines who counts as bearing its costs.').

omega_variable(
    epsilon_regime_variability,
    'Extraction for this reading varies widely across state contexts (estimated 0.25-0.45): does the composite value mask rope-like and snare-like regimes behind a single number?',
    'Decompose into per-regime variant stories (rentier-monarchy variant, republican-authoritarian variant, parliamentary-Islamic variant) each with its own epsilon, stakeholders, and measurement series.',
    'In high-coercion variants the payer seats'' effective extraction approaches snare levels; in low-coercion parliamentary variants the arrangement computes closer to rope. The composite claimed type remains tangled_rope but per-seat verdicts diverge sharply by variant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_regime_variability, empirical, 'Cross-regime variance in the hybrid''s extraction and coercion profile.').

omega_variable(
    legitimacy_yield_trajectory,
    'Is the symbolic sharia layer still purchasing genuine legitimacy, or has its yield collapsed into pure display (theater_ratio crossing 0.5)?',
    'Longitudinal survey series on regime religiosity credibility; discourse analysis of opposition framing that accuses rulers of cherry-picking God''s law; participation data for official religious events versus independent ones.',
    'If the yield has collapsed, the constraint drifts piton-ward - maintained by inertia and performance with no party collecting real legitimacy - and the theater_ratio series dates the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_yield_trajectory, empirical, 'Whether the hybrid''s symbolic component still functions as legitimation or only as display.').

omega_variable(
    scholar_quiescence_mechanism,
    'Is traditionalist quiescence under the hybrid produced by structural coercion (teaching licenses, endowment control, prosecution risk) or by institutional identity fusion (state service having become constitutive of official scholarly identity)?',
    'Post-liberalization trajectory: if scholars reassert independent authority rapidly when coercion relaxes, suppression was structural; if they voluntarily defend the state''s selection boundary, the fusion is internalized.',
    'Internalized fusion raises effective suppression above the structural measure and predicts persistence of the arrangement even after regime change; structural-only suppression predicts rapid re-traditionalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_quiescence_mechanism, empirical, 'Structural versus internalized suppression of the traditionalist flank.').

omega_variable(
    flank_suppression_asymmetry,
    'Do the two victim flanks bear symmetric costs, or does suppression tilt with the regime''s coalition (states allied with traditionalist establishments prosecuting reformists harder, and vice versa)?',
    'Comparative coding of prosecution rates, licensing denials, and censorship incidents against regime coalition composition across hybrid states.',
    'Asymmetric suppression shifts the relative directionality of the two payer seats and changes which flank dominates the measured resistance; the composite hides which flank is the operative bearer of costs in any given regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flank_suppression_asymmetry, empirical, 'Coalition-dependent asymmetry between traditionalist and reformist cost-bearing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 1924, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhs_state_hybrid_tr_t1924, quran_hadith_substrate__state_hybrid, theater_ratio, 1924, 0.18).
narrative_ontology:measurement(qhs_state_hybrid_tr_t1936, quran_hadith_substrate__state_hybrid, theater_ratio, 1936, 0.22).
narrative_ontology:measurement(qhs_state_hybrid_tr_t1948, quran_hadith_substrate__state_hybrid, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(qhs_state_hybrid_tr_t1960, quran_hadith_substrate__state_hybrid, theater_ratio, 1960, 0.28).
narrative_ontology:measurement(qhs_state_hybrid_tr_t1972, quran_hadith_substrate__state_hybrid, theater_ratio, 1972, 0.31).
narrative_ontology:measurement(qhs_state_hybrid_tr_t1984, quran_hadith_substrate__state_hybrid, theater_ratio, 1984, 0.36).
narrative_ontology:measurement(qhs_state_hybrid_tr_t1996, quran_hadith_substrate__state_hybrid, theater_ratio, 1996, 0.4).
narrative_ontology:measurement(qhs_state_hybrid_tr_t2008, quran_hadith_substrate__state_hybrid, theater_ratio, 2008, 0.44).
narrative_ontology:measurement(qhs_state_hybrid_tr_t2024, quran_hadith_substrate__state_hybrid, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(qhs_state_hybrid_be_t1924, quran_hadith_substrate__state_hybrid, base_extractiveness, 1924, 0.2).
narrative_ontology:measurement(qhs_state_hybrid_be_t1936, quran_hadith_substrate__state_hybrid, base_extractiveness, 1936, 0.24).
narrative_ontology:measurement(qhs_state_hybrid_be_t1948, quran_hadith_substrate__state_hybrid, base_extractiveness, 1948, 0.27).
narrative_ontology:measurement(qhs_state_hybrid_be_t1960, quran_hadith_substrate__state_hybrid, base_extractiveness, 1960, 0.29).
narrative_ontology:measurement(qhs_state_hybrid_be_t1972, quran_hadith_substrate__state_hybrid, base_extractiveness, 1972, 0.31).
narrative_ontology:measurement(qhs_state_hybrid_be_t1984, quran_hadith_substrate__state_hybrid, base_extractiveness, 1984, 0.35).
narrative_ontology:measurement(qhs_state_hybrid_be_t1996, quran_hadith_substrate__state_hybrid, base_extractiveness, 1996, 0.37).
narrative_ontology:measurement(qhs_state_hybrid_be_t2008, quran_hadith_substrate__state_hybrid, base_extractiveness, 2008, 0.39).
narrative_ontology:measurement(qhs_state_hybrid_be_t2024, quran_hadith_substrate__state_hybrid, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(qhs_state_hybrid_su_t1924, quran_hadith_substrate__state_hybrid, suppression_requirement, 1924, 0.44).
narrative_ontology:measurement(qhs_state_hybrid_su_t1936, quran_hadith_substrate__state_hybrid, suppression_requirement, 1936, 0.52).
narrative_ontology:measurement(qhs_state_hybrid_su_t1948, quran_hadith_substrate__state_hybrid, suppression_requirement, 1948, 0.34).
narrative_ontology:measurement(qhs_state_hybrid_su_t1960, quran_hadith_substrate__state_hybrid, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(qhs_state_hybrid_su_t1972, quran_hadith_substrate__state_hybrid, suppression_requirement, 1972, 0.33).
narrative_ontology:measurement(qhs_state_hybrid_su_t1984, quran_hadith_substrate__state_hybrid, suppression_requirement, 1984, 0.51).
narrative_ontology:measurement(qhs_state_hybrid_su_t1996, quran_hadith_substrate__state_hybrid, suppression_requirement, 1996, 0.44).
narrative_ontology:measurement(qhs_state_hybrid_su_t2008, quran_hadith_substrate__state_hybrid, suppression_requirement, 2008, 0.56).
narrative_ontology:measurement(qhs_state_hybrid_su_t2024, quran_hadith_substrate__state_hybrid, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% The colloquial label 'sharia in modern states' conflates three structurally distinct constraints (epsilon-invariance decomposition of the quran_hadith_substrate kernel): traditionalist_taqlid (madhhab consensus binding on believers), state_hybrid (this file - sovereign selection of rulings by domain), and reformist_ijtihad (ethics-and-maslaha-driven contextual reinterpretation). Each carries its own epsilon, beneficiary/victim structure, and enforcement profile. The state_hybrid reading structurally influences both siblings: it controls scholarly patronage, teaching licensure, and the official interpretive platform, reshaping the operating environment of madhhab institutions and reformist scholarship without resolving the underlying contest. Upstream/downstream is directional: state adoption decisions change resource availability FOR both sibling communities; neither sibling controls the state's selection boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
