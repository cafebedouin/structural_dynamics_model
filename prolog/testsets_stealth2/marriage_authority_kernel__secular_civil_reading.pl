% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Reading of Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative law / constitutional pluralism / religious governance
 *
 * SUMMARY:
 *   In India's pluralist family-law order, this story instantiates the
 *   secular-civil reading of the marriage-authority kernel: marriage and
 *   family-law authority derive from the Special Marriage Act 1954 and
 *   constitutional individual rights, adjudicated by civil courts. A genuine
 *   coordination function (a religion-neutral marriage path, the highest
 *   statutory gender equity of any reading, inter-religious marriage without
 *   conversion) coexists with a real extraction vector running through the
 *   same structure: the thirty-day public notice transfers information and
 *   intervention leverage to kin networks before the marriage is legally
 *   secure, and completing a civil marriage carries persistent social costs
 *   of exiting community law. The claim and the metrics are independent
 *   authored facts: claimed_type is tangled_rope from the structure (real
 *   coordination plus asymmetric, actively enforced extraction with
 *   identifiable payers); the metrics describe observed operation. The
 *   epsilon referent is the standing SMA arrangement as this reading assesses
 *   it — the reading endorses the arrangement's premise while its own
 *   tradition (Law Commission papers, High Court orders) documents the notice
 *   defect, so epsilon is moderate rather than near-zero. The four sibling
 *   readings are separate constraint files linked through the network block;
 *   their victim sets, forums, and equity profiles differ structurally and
 *   are not averaged into this file.
 *
 * KEY AGENTS:
 *   - parliament_of_india: Agenda-setter (institutional/arbitrage) — authored the act, retains amendment power, hosts the uniform-code contest
 *   - supreme_court_high_courts: Adjudicative agenda-setter (institutional/constrained) — absorbs drift between statutory text and constitutional rights
 *   - marriage_officers_district_registry: Administrative agenda-setter (moderate/mobile) — controls local notice handling and thus couples' exposure
 *   - interfaith_couples: Intended beneficiary and simultaneous payer (powerless/trapped) — the sole non-conversion route, exposed by the notice window
 *   - women_exiting_patriarchal_personal_law: Beneficiary-payer (powerless/trapped) — gains exit terms unavailable under community regimes
 *   - notice_window_couples: Primary payers (powerless/trapped) — bear surveillance and coercion during the thirty-day window
 *   - community_exit_couples: Payers (moderate/identity_locked) — carry ostracism costs that persist after registration
 *   - natal_kin_networks: Beneficiaries of the notice window (organized/mobile) — receive advance word and intervention time
 *   - personal_law_boards_and_clergy: Excluded (organized/mobile) — parallel jurisdictions outside the secular coalition
 *   - civil_liberties_lawyers: Analytical observer (moderate/analytical) — litigate the notice provision and build the correction record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.44).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Reading of Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative law / constitutional pluralism / religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'c52053df-19f2-459c-af65-12cf77ca9f2d').
narrative_ontology:cs_kernel_codification('c52053df-19f2-459c-af65-12cf77ca9f2d', fixed_text).
narrative_ontology:cs_authority_grounding('c52053df-19f2-459c-af65-12cf77ca9f2d', lineage).
narrative_ontology:cs_interpretation_layer_present('c52053df-19f2-459c-af65-12cf77ca9f2d').
narrative_ontology:cs_reading_relation('c52053df-19f2-459c-af65-12cf77ca9f2d', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('c52053df-19f2-459c-af65-12cf77ca9f2d', marriage_authority_kernel__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('c52053df-19f2-459c-af65-12cf77ca9f2d', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c52053df-19f2-459c-af65-12cf77ca9f2d', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('c52053df-19f2-459c-af65-12cf77ca9f2d', foundational, marital_validity_from_individual_consent).
narrative_ontology:cs_axiom_status(marital_validity_from_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('c52053df-19f2-459c-af65-12cf77ca9f2d', marital_validity_from_individual_consent, deontological).
narrative_ontology:cs_axiom('c52053df-19f2-459c-af65-12cf77ca9f2d', foundational, civil_court_exclusive_family_jurisdiction).
narrative_ontology:cs_axiom_status(civil_court_exclusive_family_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('c52053df-19f2-459c-af65-12cf77ca9f2d', civil_court_exclusive_family_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('c52053df-19f2-459c-af65-12cf77ca9f2d', constitutional_individual_rights_supremacy).
narrative_ontology:cs_drift_state('c52053df-19f2-459c-af65-12cf77ca9f2d', contemporary_digital_notice_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c52053df-19f2-459c-af65-12cf77ca9f2d', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_exiting_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, notice_window_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, community_exit_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, natal_kin_networks).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, women_exiting_patriarchal_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, religious_neutral_civil_marriage_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Special Marriage Act in 1954 to provide a religion-neutral marriage path alongside the personal laws, and retains sole power to amend it. Successive governments have received proposals to make the notice procedure confidential and have declined, citing coalition sensitivities around family-law reform; the chamber currently hosts the wider uniform-civil-code contest.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, parliament_of_india, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicates validity, divorce, maintenance, and custody for civil marriages under constitutional standards, and issues protection orders for couples threatened during the notice window. Several High Courts have restrained registry offices from publishing notice lists online and have read the procedure narrowly; these interpretations move day-to-day practice without textual amendment.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, supreme_court_high_courts, agenda_setter,
    institutional, generational, constrained, national).

% Receive the couple's notice, post it at the office (and, in many districts until recent orders, on official websites), invite objections for thirty days, then solemnize or register the marriage. Their local handling — who is told, how lists are stored, whether objections are verified — determines how much advance warning families and neighbors receive.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, marriage_officers_district_registry, agenda_setter,
    moderate, biographical, mobile, national).

% For two citizens of different religions who will not convert, the civil act is the only lawful route to marriage. Filing starts a thirty-day public window in which parents, employers, landlords, and officials can learn of the plan; abandoning the filing means no marriage, and switching to a personal law means one partner's conversion. After registration they hold a marriage no community tribunal controls.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_couples, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, interfaith_couples, payer).

% The civil code offers divorce grounds, maintenance rights, and consent requirements that community regimes reserve or deny, so it functions as the main exit route from unilateral communal prerogatives. Using it announces the exit publicly during the notice window and marks the woman socially afterward; returning to the community's framework afterward is rarely available.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_exiting_patriarchal_personal_law, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, women_exiting_patriarchal_personal_law, payer).

% Couples whose filings become known during the thirty days: family members trace them through the posted notice, police visit at relatives' urging, employers and landlords are informed, and some couples face confinement or violence before the marriage matures into a legal fact. The window ends only when registration completes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, notice_window_couples, payer,
    powerless, immediate, trapped, regional).

% Couples who complete a civil marriage stand outside both extended families' communal frameworks: invitations stop, community dispute-resolution and festival life close off, and children grow up between frameworks. These costs begin at registration and persist for decades; nothing in the civil act addresses them.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, community_exit_couples, payer,
    moderate, biographical, identity_locked, regional).

% Extended families and caste or community networks learn of an intended civil marriage through the posted notice weeks before it becomes final. The interval lets them locate the couple, involve police, negotiate, or stage a reconciliation; after registration their leverage largely ends.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, natal_kin_networks, beneficiary,
    organized, generational, mobile, regional).

% Religious leaderships run parallel marriage and divorce jurisdictions under their own acts and customs. They sit outside the coalition that maintains the civil path, oppose its expansion into a common code, and advise constituents accordingly; their constituents can simply never use the civil act, so their position needs no accommodation from it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, personal_law_boards_and_clergy, excluded,
    organized, generational, mobile, national).

% Bring challenges to notice-list publication, obtain protection orders and police escorts for couples in the window, and build the case record that higher courts draw on. Their docket is the main channel through which practice on the ground reaches the adjudicative seats.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_liberties_lawyers, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, natal_kin_networks).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one civil marriage pathway open to citizens regardless of religion: state registration, civil-court adjudication of validity, divorce, and maintenance, and legal recognition for marriages no personal law would solemnize — solving the collective problem that four parallel communal jurisdictions left cross-community couples with no lawful route except conversion.
% TRANSFER_FUNCTION: Moves marital-jurisdiction authority from communal institutions to the state; moves marriage governance from community norms to individual statutory consent; and, through the thirty-day public notice, moves information about an intended marriage from the couple to kin networks, neighbors, and officials before the marriage is legally secure.
% ABSENT_VOICES: Personal-law boards and clergy are structurally outside the secular coalition and would contest its premises; the couples themselves hold no formal seat during the notice window — no counsel of record, no confidentiality guarantee — so the people most affected by the notice provision enter the conversation only after harm, through litigation.
% DISAPPEARANCE_RATIONALE: Inter-religious couples would lose the only non-conversion route to lawful marriage; pending filings would collapse; the displaced demand would flow into conversion-under-personal-law workarounds or foreign registration; the constitutional commitment to a civil marriage option would lose its institutional embodiment, and the uniform-code debate would lose its working prototype.
% FOUNDING_PROBLEM: After Partition, India needed a marriage form available across religious lines and to citizens rejecting communal gatekeeping: personal laws made cross-community marriage legally impossible without conversion, and women's exit options were governed by unilateral communal prerogatives. The Special Marriage Act 1954 was built to supply a religion-neutral civil path with court adjudication.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: Law Commission of India consultation papers on family-law reform treat cross-community marriage access as an unsolved problem; Supreme Court and multiple High Court opinions (protection orders, notice-publication restraints) attest that the danger window persists; independent family-law scholarship documents conversion-to-marry workarounds as evidence the founding gap remains. No attestation comes from the couples alone.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.44: the reading endorses the arrangement, but assessed by its own lights the notice window is a condemned defect — extraction concentrates on a minority of civil-path users yet is severe for them (tracking, police visits, violence), and the exit-ostracism costs fall on every couple that completes the path. The series rises from 0.22 (1954, paper-era notices, limited reach) through the communal-mobilization decades to a 2021 peak (0.47) when notice lists circulated digitally and via information requests, retreating slightly by 2026 (0.44) under judicial curbs on publication. Suppression 0.52: the act forecloses quiet civil marriage — every civil marriage passes the public gate — while leaving all four personal-law alternatives fully available, so suppressive force is real but bounded. Theater 0.20: adjudication and registration are substantially functional; the performative share is small and grows only slowly. Accessibility_collapse 0.28: alternatives do not collapse — the sibling jurisdictions remain one filing away; alternatives collapse only for conversion-averse interfaith couples, for whom the civil path is the sole remaining route. Resistance 0.55: sustained litigation against notice publication plus intense political contestation over expansion. The temporal series run on one shared eight-point grid (every tracked metric authored at every point) so no end-state value is substituted into earlier rows. The suppression_requirement series is authored deliberately because the story tracks enforcement-capacity change: an administrative ratchet (office posting, then online publication, then list circulation) followed by partial judicial rollback — not a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats experience the arrangement as constitutional promise and routine administration: parliament sees a working prototype of the uniform-code direction, the courts see doctrine they can steer. The payer seats experience the identical statutory text as exposure: the same thirty days that read as verification from the bench read as a manhunt from inside the window. The excluded seats (boards, clergy) experience the reading's expansion as jurisdictional dispossession. Same statute, different computed types per seat — the engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (interfaith_couples, women_exiting_patriarchal_personal_law) derive low directionality — the arrangement subsidizes them with the only non-conversion route and equitable exit terms. Declared victims (notice_window_couples, community_exit_couples) derive high directionality — they bear the window's coercion costs and the post-registration ostracism, with trapped or identity-locked exit pushing them toward the full-target end. The dual-positioned couples sit mid-range: the derivation reads their beneficiary declaration and damps their effective burden, but the operative costs fall precisely on them; this residual is flagged here rather than corrected through a directionality override because the override surface is keyed to power atoms, and a powerless-class override would misstate the women-exit group, which is a genuine beneficiary. Natal_kin_networks are organized beneficiaries with mobile exit — the nearest-to-subsidy seat among organized actors, collecting the notice window's intervention leverage. Personal-law boards are excluded rather than coordinated: they sit outside the transfer surface entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — cross-community marriage remains legally fraught and the conversion workaround persists — so no mandatrophy resolution is declared and none is warranted. The tangled_rope classification guards both error directions: reading the arrangement as pure coordination would erase the identifiable payers of the notice window and the documented coercion it enables; reading it as pure extraction would erase the coordination no sibling reading supplies (non-conversion interfaith marriage, court-adjudicated gender-equitable divorce and maintenance). If the notice provision were amended to confidential verification, the extraction leg would drop and the structure would drift toward pure coordination — a trajectory tracked by the notice_publicity_separability and judicial_protection_generalization omegas rather than pre-declared here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'How would this constraint''s victim set, adjudicative forum, and equity profile change under each sibling reading of the marriage_authority_kernel?',
    'Comparative compilation across the five sibling stories: align victim sets, forums, enforcement modes, and measured epsilon side by side.',
    'Classification is per-reading; a merged analysis would average away the notice-window burden unique to this instantiation and misattribute communal-enforcement burdens to it or vice versa.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'This story is one reading of the marriage-authority kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    notice_publicity_separability,
    'Is the thirty-day notice''s protective function (fraud, bigamy, and capacity verification) separable from its publicity (posting at the office, formerly online publication)?',
    'Natural experiment from districts and judgments adopting confidential verification (sealed notices, in-camera objection handling): if fraud detection holds while exposure falls, the functions are separable.',
    'If separable, the extraction leg is removable by ordinary amendment and the structure drifts toward pure coordination; if inseparable, part of the measured burden is the price of the verification function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_publicity_separability, empirical, 'Whether notice publicity is load-bearing for the act''s verification function.').

omega_variable(
    exit_cost_attribution,
    'Do the social costs of leaving community marriage law belong to this reading''s arrangement or to the community regimes being exited from?',
    'Counterfactual comparison of ostracism outcomes for couples marrying under the civil act against family wishes versus equivalently situated couples marrying under personal law against family wishes, isolating the marginal exposure created by the civil path''s publicity.',
    'If the costs are attributable to sibling readings'' enforcement, this reading''s epsilon falls materially; if the civil path''s publicity adds marginal exposure beyond the communal baseline, epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_attribution, conceptual, 'Attribution of exit costs between the civil path and the communal regimes it exits.').

omega_variable(
    judicial_protection_generalization,
    'Will judicial restraints on notice-list circulation generalize into standard registration practice, or remain patchy across districts?',
    'Track High Court orders and district-level registration practice over the next decade; measure variance in publication practice across registries.',
    'Generalization would pull the suppression and extractiveness series down and date a transition toward pure coordination; patchiness sustains the tangled structure indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_protection_generalization, empirical, 'Trajectory of judicially imposed confidentiality protections on the notice procedure.').

omega_variable(
    ucc_terminal_configuration,
    'Is the secular reading a transitional step toward a mandatory uniform civil code, or a permanently parallel option within durable legal pluralism?',
    'Statutory and political trajectory of Article 44 implementation; whether parallel personal-law jurisdictions are retained after any uniform-code enactment.',
    'A transitional trajectory would introduce sunset-like dynamics (without a declared clause) and reframe the arrangement''s justification as transition rather than steady state; durable parallelism preserves the tangled-rope reading as terminal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_terminal_configuration, conceptual, 'Whether the civil path is destined to absorb the personal-law readings or merely coexist with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement_basis(marr_tr_t1954, observed).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1970, 0.11).
narrative_ontology:measurement_basis(marr_tr_t1970, observed).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1985, 0.13).
narrative_ontology:measurement_basis(marr_tr_t1985, observed).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement_basis(marr_tr_t1995, observed).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(marr_tr_t2005, observed).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement_basis(marr_tr_t2015, observed).
narrative_ontology:measurement(marr_tr_t2021, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2021, 0.19).
narrative_ontology:measurement_basis(marr_tr_t2021, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(marr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement_basis(marr_be_t1954, observed).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement_basis(marr_be_t1970, observed).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement_basis(marr_be_t1985, observed).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement_basis(marr_be_t1995, observed).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement_basis(marr_be_t2005, observed).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(marr_be_t2015, observed).
narrative_ontology:measurement(marr_be_t2021, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2021, 0.47).
narrative_ontology:measurement_basis(marr_be_t2021, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2026, 0.44).
narrative_ontology:measurement_basis(marr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement_basis(marr_su_t1954, observed).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1970, 0.36).
narrative_ontology:measurement_basis(marr_su_t1970, observed).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement_basis(marr_su_t1985, observed).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1995, 0.46).
narrative_ontology:measurement_basis(marr_su_t1995, observed).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement_basis(marr_su_t2005, observed).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(marr_su_t2015, observed).
narrative_ontology:measurement(marr_su_t2021, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement_basis(marr_su_t2021, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(marr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Indian marriage law' conflates five structurally distinct authority arrangements: this secular-civil reading plus four personal-law readings (Hindu codified, Muslim shariat, Christian canonical, Parsi communal). Per the epsilon-invariance principle they are authored as separate stories sharing the marriage_authority_kernel, each with its own epsilon, victim set, and forum; this file links to all four siblings. Influence runs outward from this reading: its constitutional-individual-rights premise supplies the normative vocabulary with which sibling readings are increasingly challenged (most sharply the muslim_shariat_reading), altering their legitimacy conditions without eliminating them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
