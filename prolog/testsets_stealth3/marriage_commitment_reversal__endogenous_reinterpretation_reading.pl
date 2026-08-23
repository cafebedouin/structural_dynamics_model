% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: 1890 Official Declaration as Revelation-Legitimated Reversal (Endogenous Reinterpretation Reading)
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   On 25 September 1890 Wilford Woodruff issued the Official Declaration
 *   suspending the solemnization of new plural marriages, presenting it as
 *   divine direction received after prayer, and the October conference
 *   sustained it by common consent; the text entered canon in 1908. This
 *   story models the arrangement THIS reading sees: a reversal whose
 *   operative warrant is internal revelation, in which the living oracle
 *   legitimately reinterprets God's will under changed circumstances, member
 *   assent is bound through the sustaining mechanism, the prophetic office's
 *   interpretive monopoly is preserved and demonstrated, and Section 132
 *   remains canonically intact while practice stops. ASSUMPTIONS STATED: (1)
 *   the arrangement is modeled as church-internal; federal legal pressure
 *   enters only as the changed circumstance the revelation addresses, not as
 *   the operative cause, because the claim that external coercion was the
 *   operative cause belongs to the sibling story, not this one; (2) epsilon's
 *   referent is the standing arrangement under contest, the declaration
 *   together with its sustaining mechanism and enforcement of assent,
 *   assessed by this reading's own lights, never the reading-endorsed
 *   alternative; (3) the interval 1890-1910 covers issuance, statehood, and
 *   the enforcement ratchet through the Smoot-hearing era. KEY AGENTS (by
 *   structural relationship):
 *
 * KEY AGENTS:
 *   - first_presidency: primary beneficiary and agenda setter (institutional / identity_locked) — authors the declaration, controls its presentation and the teaching of Section 132, collects the legitimacy dividend
 *   - quorum_of_twelve: secondary beneficiary with a paying minority (institutional / identity_locked) — witnesses the direction; dissenters pay with their offices
 *   - plural_marriage_covenant_families: principal target (moderate / identity_locked) — bears household dissolution pressure, stigma, and the re-explanation of the covenant their sacrifices purchased
 *   - manifesto_objectors_and_continuing_sealers: target (moderate / trapped) — pays by suspension, investigation, and excommunication
 *   - rank_and_file_latter_day_saints: dual-positioned beneficiary-payer (organized / identity_locked) — keeps the surviving community, carries the unanswered why-question
 *   - utah_statehood_coalition: incidental beneficiary (powerful / mobile) — converts the declaration into statehood and exits the question
 *   - religious_historians: analytical observer — sees the full documentary structure across narrative versions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.75).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "1890 Official Declaration as Revelation-Legitimated Reversal (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'accf19ca-385d-41e4-9844-6a6a8097e448').
narrative_ontology:cs_kernel_codification('accf19ca-385d-41e4-9844-6a6a8097e448', fixed_text).
narrative_ontology:cs_authority_grounding('accf19ca-385d-41e4-9844-6a6a8097e448', lineage).
narrative_ontology:cs_interpretation_layer_present('accf19ca-385d-41e4-9844-6a6a8097e448').
narrative_ontology:cs_reading_relation('accf19ca-385d-41e4-9844-6a6a8097e448', marriage_commitment_reversal__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('accf19ca-385d-41e4-9844-6a6a8097e448', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('accf19ca-385d-41e4-9844-6a6a8097e448', foundational, living_oracle_supersedes_prior_command).
narrative_ontology:cs_axiom_status(living_oracle_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('accf19ca-385d-41e4-9844-6a6a8097e448', living_oracle_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('accf19ca-385d-41e4-9844-6a6a8097e448', secondary, command_validity_is_context_bound).
narrative_ontology:cs_axiom_status(command_validity_is_context_bound, holdable).
narrative_ontology:cs_axiom_grounding('accf19ca-385d-41e4-9844-6a6a8097e448', command_validity_is_context_bound, theological).
narrative_ontology:cs_reference_frame('accf19ca-385d-41e4-9844-6a6a8097e448', living_oracle_contextual_supersession).
narrative_ontology:cs_drift_state('accf19ca-385d-41e4-9844-6a6a8097e448', smoot_hearings_second_declaration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('accf19ca-385d-41e4-9844-6a6a8097e448', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, first_presidency).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, utah_statehood_coalition).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_marriage_covenant_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, manifesto_objectors_and_continuing_sealers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_latter_day_saints).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wilford Woodruff and his counselors issue the Official Declaration of September 1890, present it as direction received from the Lord after prolonged prayer, schedule the conference sustaining vote, and control the Deseret News and conference platforms through which the declaration is explained. They retain sole discretion over what counts as revelation and over how Section 132 is taught thereafter. The office's warrant rests on the claim that the change came from God through the presiding council; from inside the office, conceding any other origin for the reversal is not a survivable move.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, first_presidency, agenda_setter,
    institutional, generational, identity_locked, global).

% Share in announcing and defending the declaration and gain standing as witnesses to the new direction. A minority judge the earlier commandments still binding and continue performing sealings privately for years; two of their number are ultimately dropped from the council for that course, and others carry private doubts recorded only in diaries.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve, payer).

% Entered plural marriage between the 1840s and 1880s as the highest covenant then taught, frequently at great personal cost, and built households whose legal, economic, and eternal footing rested on it. After 1890 the surrounding posture shifts: husbands face pressure to confine themselves to one household, wives in plural households see their standing downgraded in sermons and lesson materials, children carry stigma, and the promise the sacrifices were made for is re-explained rather than honored. Remaining in the community means absorbing the reversal; leaving means forfeiting the sealed-family bonds and the salvation framework their lives are organized around.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_marriage_covenant_families, payer,
    moderate, generational, identity_locked, national).

% Members and a few General Authorities who conclude that a command God gave cannot be withdrawn by circumstance keep performing or entering plural marriages after the declaration. They face investigation, suspension, and after 1904 excommunication; some gather in separate congregations in northern Mexico, Alberta, and the Intermountain West. Their exit from the main body runs through expulsion.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, manifesto_objectors_and_continuing_sealers, payer,
    moderate, biographical, trapped, continental).

% Sustain the declaration by uplifted hand at the October 1890 conference and adjust their understanding accordingly. They keep their temples, congregations, and covenant infrastructure intact, gain relief from prosecution and harassment, and receive a demonstration that the living prophet's word governs over past scripture. They also inherit the theological puzzle of a command given and withdrawn, absorb decades of sermons reframing plural marriage as temporary, and have no standing forum in which the question of why God's will changed is debated before the vote is taken.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_latter_day_saints, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_latter_day_saints, payer).

% Territorial delegates, business leaders, and Washington lobbyists who had tied Utah's admission to the plural-marriage question use the declaration to reopen the statehood campaign, which succeeds in January 1896. They operate in electoral politics and can redirect effort to other obstacles; the declaration removes the one obstacle they could not remove themselves.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, utah_statehood_coalition, beneficiary,
    powerful, biographical, mobile, continental).

% Scholars working from Woodruff's diaries, the Reed Smoot hearing transcript, and later-released records reconstruct what happened in September 1890 and in the enforcement years that followed. They hold no stake in the covenant structure and can compare accounts freely; their publications periodically force the institution to answer for discrepancies between the circulated narrative and the documentary record.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, religious_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, first_presidency).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: An embattled covenant community facing legal annihilation needed a single authoritative resolution that every member could accept simultaneously without fracturing: one declaration, delivered through the established revelatory channel and ratified by one sustaining vote, replaced thousands of private decisions about whether to keep entering plural marriages. The same mechanism supplied a shared account of why the practice stopped that members could carry without abandoning the rest of their belief structure.
% TRANSFER_FUNCTION: Moves narrative authority and assent upward: members surrender independent judgment on the reversal question to the presiding councils, and the presidency's interpretive monopoly over God's will is confirmed and extended. Moves practical burdens downward and outward: household dissolution pressure, stigma, and disciplinary exposure concentrate on plural families and on those who refuse the new direction, while institutional survival benefits flow to the whole body and the legitimacy dividend concentrates in the First Presidency.
% ABSENT_VOICES: Women in plural marriages decided nothing and were informed of outcomes after the fact; their diaries and testimonies sit outside the record the decision drew on. Federal judges, prosecutors, and commissioners possessed a causal account of the reversal (compliance produced under legal duress) that this reading's framework assigns no standing; their voice enters history only through hostile venues such as the Smoot hearings. Objecting apostles spoke in private diaries rather than on the conference stand.
% DISAPPEARANCE_RATIONALE: If the declaration and its sustaining-and-enforcement apparatus vanished overnight, the church faced disincorporation under the Edmunds-Tucker Act's pending machinery, seizure of temple funds and property, and a split between a compliance faction and a covenant-fidelity faction with rival claimants to authority. Utah's statehood timeline slips indefinitely; the sealed-covenant expectations of hundreds of families keep their pre-1890 shape; the precedent pattern by which the institution later re-prices other policies through revelation does not exist in this form.
% FOUNDING_PROBLEM: How to stop a commanded practice without repudiating the source of the command: the community needed to comply enough with federal law to survive as a legal corporation while preserving the doctrine that God speaks through a living oracle and that prior commandments were genuinely divine when given.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside the benefiting parties: federal officials' own enforcement record (seizure suits, temple-fund targets, disfranchisement decrees) attests that the legal threat was genuine, and later academic histories confirm the community's existential jeopardy. What only the benefiting parties attest is that the resolution arrived as revelation rather than as capitulation; dissenting apostles and the fundamentalist movement explicitly deny that part. Mass sincere assent in the sustaining votes is real but comes from inside the arrangement.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends moderate (0.58): assent is compelled socially rather than by argument, costs concentrate on plural families and dissenters while the narrative forecloses the why-question, but the arrangement delivers a real collective good (communal survival with covenant infrastructure intact), which tempers the measure. Suppression (0.75 end) is authored as a RAW STRUCTURAL property and is deliberately NOT scaled by power or scope in this authoring; the engine owns any scaling arithmetic. The suppression series is authored because the story's narrative specifically tracks enforcement-capacity change: the ratchet from the 1890 declaration through the 1904 second declaration, the dropping of two apostles in 1905-06, and excommunications of continuing practitioners through 1910 is an enforcement-infrastructure build-up, not mere extraction drift. Theater_ratio (0.45 end) reflects a mixed narrative: the vision account is sincerely accepted by most members and does functional work, yet the account also performs cover, and it grows more elaborate in later retellings than the September 1890 diary entries; the series rises as the narrative hardens under external scrutiny. Accessibility_collapse 0.5: rejection paths exist and are taken (schism communities persist), but the price of taking them inside the community is total, so alternatives collapse halfway rather than completely. Resistance 0.45: sustained objection from a minority of apostles and laity, plus durable fundamentalist persistence, against mass sincere assent. All three tracked series run on ONE SHARED TIME GRID (1890, 1893, 1896, 1899, 1902, 1905, 1908, 1910) so no metric row borrows another's endpoints. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is asserted from structural belief (a genuine collective-survival coordination function AND asymmetric extraction through the same structure, actively enforced); the metrics are authored independently as descriptive truths of the arrangement's actual operation, and any divergence the engine computes is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute different types from identical structural inputs. From the First Presidency's chair the arrangement is the revelatory process functioning as designed: prayer, answer, declaration, ratification, and the office's authority confirmed by the very act of superseding an earlier command. From the plural-family chairs the same structure operates as covenant revision without consultation: the promise their sacrifices purchased is re-priced by the same authority that sold it, with no forum in which to ask why. The dissenting-apostle chair experiences betrayal of a command they regarded as eternal. The rank-and-file chair is split: gratitude for institutional survival alongside an inherited, undebatable question. The historian chair sees the widening distance between the terse September 1890 diary entries and the elaborated public narrative. The engine computes this divergence from power atoms, exit options, and directionalities; this commentary does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency declares as beneficiary and holds agenda-setting power with identity-locked exit: derivation places it near the beneficiary pole, and its identity lock keeps effective extraction there from being read as borne cost, since the office cannot concede an alternative causal account without dissolving its own warrant. The Twelve derive near-beneficiary with slight upward pull from the paying minority. Plural covenant families declare as victims with identity-locked exit: derivation drives them toward the full-target end, and the identity lock amplifies effective extraction relative to a mobile target, since exit forfeits the sealed-family salvation framework itself. Continuing sealers are victims with trapped exit: maximal target-side weighting. Rank-and-file saints hold dual declarations (beneficiary with payer secondary): derivation lands them mid-range, near symmetric. The statehood coalition is a mobile beneficiary whose arbitrage-grade exit damps its effective extraction toward the subsidy end. Religious historians are analytical seats. National-to-global scopes apply modest verification-cost amplification through the engine's scope modifier; suppression passes through unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric mislabels. Reading the declaration as pure coordination (rope) ignores that the same structure which saved the community transferred its costs onto plural families and dissenters while routing the decision around every forum where the why-question could be asked, and that persistence depends on actively enforced assent rather than participant preference. Reading it as pure extraction (snare) ignores that the coordination function was real and load-bearing: without a single authoritative reversal the community plausibly fragments under federal assault, and the fragmenting factions would have destroyed the covenant infrastructure both camps valued. Mandatrophy: the founding problem, reconciling command-continuity with legal survival under a living-oracle theology, remains LIVE, since the same pattern recurs whenever the institution re-prices a previously commanded or entrenched practice through the revelatory register; the mandate has not outlived its function, no sunset clause exists, and mandatrophy_resolved is correctly left unset. If the identity frame broke, if the institution ever conceded that reversals track external pressure rather than revelation, the arrangement's warrant collapses and the victim set widens to include every member who assented under the prior account; that counterfactual is the boundary between this reading and its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'How would this constraint''s structure change under the sibling readings of kernel marriage_commitment_reversal?',
    'Compare the compiled sibling stories (marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap) on the shared metric surface; the location and sign of their divergences identify where the readings actually disagree structurally.',
    'Under the exogenous_override_reading the victim set widens to all assenting members (causation misrepresented to them), extractiveness rises, and the type leans toward enforced extraction; under the practice_doctrine_gap reading the arrangement registers as unresolved persistence with elevated theatrical maintenance and reduced enforcement necessity. This story''s authored values are valid only for the endogenous reading; they are not averages across the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexed classification of a three-way kernel: this story instantiates the endogenous_reinterpretation_reading, and its epsilon, victim set, and theater composition are committed to that reading.').

omega_variable(
    woodruff_vision_account_stability,
    'Did the account of the September 23, 1890 vision remain stable from Woodruff''s terse diary entries through the progressively elaborated narratives circulated in the following decade?',
    'Collate the September 1890 diary entries, contemporaneous letters, first public statements, and later retellings; measure narrative accretion across tellings.',
    'Substantial accretion raises the performative component of the arrangement, pushes theater_ratio higher than authored, and strengthens the sibling readings'' account of the reversal''s origin; a stable, terse, consistent account strengthens this reading''s warrant and lowers theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(woodruff_vision_account_stability, empirical, 'Whether the revelatory account anchoring this reading grew in the telling or was reported consistently from the outset.').

omega_variable(
    leadership_sincerity_composition,
    'What proportion of First Presidency and Twelve assent was sincere conviction that the direction was revelation, versus strategic adoption of the best available legitimating frame under mortal institutional threat?',
    'Cross-read private journals and correspondence of counselors and apostles (Cannon, Joseph F. Smith, Snow, and others) against their public statements, looking for divergence between private causal attributions and public revelatory ones.',
    'If framing dominates, the arrangement''s performative share rises sharply and its structure approaches pure enforced extraction riding a survival coordination; if conviction dominates, the authored theater_ratio is approximately right and the tangled structure is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_sincerity_composition, empirical, 'Composition of leadership assent between genuine belief in the revelation and strategic use of the revelation frame.').

omega_variable(
    sustaining_vote_deliberation_depth,
    'Was the October 1890 conference sustaining vote, and the reaffirming votes that followed, informed deliberation or ritual acclamation with no deliberative space?',
    'Examine conference proceedings for any recorded discussion, dissent opportunity, or delayed voting; compare against conferences where debate demonstrably occurred.',
    'If purely liturgical, the measured suppression reflects enforcement machinery rather than expressed member preference, and accessibility_collapse is understated relative to what member agency could actually reach; if genuine deliberative space existed and went unused, suppression is lower than the enforcement record alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustaining_vote_deliberation_depth, empirical, 'Whether member assent was informed choice or ratified formality, bearing on how suppression should be interpreted.').

omega_variable(
    theological_consistency_resolution_pathway,
    'Which frame do members adopt to resolve why God''s will changed: progressive adaptation (commands are context-bound and withdrawal is legitimate) or covenant immutability (commands are permanent and suspension is anomalous)?',
    'Track sermon corpora, lesson materials, and rates of membership attrition versus fundamentalist-schism conversion across generations after 1890.',
    'Dominant progressive-adaptation framing shrinks the long-run victim set and lets extractiveness decay toward coordination cost as the question fades; dominant immutability framing perpetuates the victim set indefinitely and fuels recurring rupture along the fundamentalist seam, keeping extractiveness elevated across generations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_consistency_resolution_pathway, conceptual, 'The unresolved why-question at the center of this reading''s victim set, and which resolution pathway the community''s teaching actually selects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_endogenous_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.18).
narrative_ontology:measurement(mcr_endogenous_tr_t1893, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1893, 0.22).
narrative_ontology:measurement(mcr_endogenous_tr_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.26).
narrative_ontology:measurement(mcr_endogenous_tr_t1899, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1899, 0.3).
narrative_ontology:measurement(mcr_endogenous_tr_t1902, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1902, 0.34).
narrative_ontology:measurement(mcr_endogenous_tr_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1905, 0.4).
narrative_ontology:measurement(mcr_endogenous_tr_t1908, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1908, 0.43).
narrative_ontology:measurement(mcr_endogenous_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.45).

% Extraction over time
narrative_ontology:measurement(mcr_endogenous_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.44).
narrative_ontology:measurement(mcr_endogenous_be_t1893, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1893, 0.47).
narrative_ontology:measurement(mcr_endogenous_be_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.5).
narrative_ontology:measurement(mcr_endogenous_be_t1899, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1899, 0.52).
narrative_ontology:measurement(mcr_endogenous_be_t1902, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1902, 0.54).
narrative_ontology:measurement(mcr_endogenous_be_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1905, 0.57).
narrative_ontology:measurement(mcr_endogenous_be_t1908, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1908, 0.58).
narrative_ontology:measurement(mcr_endogenous_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mcr_endogenous_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement(mcr_endogenous_su_t1893, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1893, 0.42).
narrative_ontology:measurement(mcr_endogenous_su_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.48).
narrative_ontology:measurement(mcr_endogenous_su_t1899, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1899, 0.52).
narrative_ontology:measurement(mcr_endogenous_su_t1902, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1902, 0.58).
narrative_ontology:measurement(mcr_endogenous_su_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1905, 0.68).
narrative_ontology:measurement(mcr_endogenous_su_t1908, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1908, 0.74).
narrative_ontology:measurement(mcr_endogenous_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% Kernel marriage_commitment_reversal decomposes into three structurally distinct constraints per the epsilon-invariance principle: the colloquial question of why the church stopped plural marriage in 1890 bundles incompatible causal attributions with different epsilon values, victim sets, and enforcement profiles. This file is the endogenous-revelation reading. The exogenous-override sibling carries higher extractiveness (misrepresentation of causation widens the victim set to all assenting members) and leans toward enforced extraction; the practice-doctrine-gap sibling carries elevated theatrical maintenance and unresolved persistence with weaker enforcement necessity. This reading sits upstream of the gap reading: the revelation narrative it installs supplies the interpretive cover that allows Section 132 to remain canon while practice remains suspended. The endogenous and exogenous core premises are mutually exclusive causal accounts of the same event and foreclose each other within any single framework; the gap reading survives under either account and is therefore shaped, not eliminated, by this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
