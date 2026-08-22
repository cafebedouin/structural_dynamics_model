% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the balanced-coexistence reading of the One
 *   Country, Two Systems kernel: the arrangement is read as a genuine,
 *   ongoing negotiation between sovereignty and autonomy in which neither
 *   side holds absolute trump, boundaries are contested rather than fixed,
 *   and resolution runs through political accommodation (case-by-case
 *   interpretation, informal bargaining, economic leverage) rather than
 *   either a treaty-enforceable autonomy guarantee or an unconditional
 *   sovereignty override. Under this reading Hong Kong's separate
 *   commercial/legal system persists because it serves a real coordination
 *   function Beijing has incentive to preserve, while the boundary of
 *   political and civil liberties has narrowed through a sequence of
 *   accommodations (2003, 2014, 2019-2020, the National Security Law) that
 *   this reading characterizes as renegotiation rather than either treaty
 *   breach or legitimate sovereign correction. The measured extraction rises
 *   across the interval (0.28→0.48) tracking a sequence of
 *   boundary-tightening episodes, each read under this frame as an
 *   accommodation shift rather than a unilateral override — that interpretive
 *   choice is exactly what distinguishes this reading from its siblings.
 *
 * KEY AGENTS:
 *   - beijing_central_authorities: agenda_setter, holds ultimate sovereign authority, negotiates the boundary case by case
 *   - hong_kong_government_administration: agenda_setter/payer, implements accommodations it does not fully author
 *   - hong_kong_business_elite and international_investors_using_hk_gateway: beneficiaries with real exit leverage that sustains the coordination function
 *   - hong_kong_pro_democracy_activists, hong_kong_press_and_civil_society: payers, trapped, bear the tightening boundary directly
 *   - hong_kong_independent_judiciary: payer/observer, retains function in commercial matters, narrowed in constitutional matters
 *   - constitutional_scholars_and_comparative_analysts: analytical observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.48).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '59927ac0-73cf-41bb-8c50-3094c227569f').
narrative_ontology:cs_kernel_codification('59927ac0-73cf-41bb-8c50-3094c227569f', fixed_text).
narrative_ontology:cs_authority_grounding('59927ac0-73cf-41bb-8c50-3094c227569f', extraction).
narrative_ontology:cs_interpretation_layer_present('59927ac0-73cf-41bb-8c50-3094c227569f').
narrative_ontology:cs_reading_relation('59927ac0-73cf-41bb-8c50-3094c227569f', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('59927ac0-73cf-41bb-8c50-3094c227569f', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('59927ac0-73cf-41bb-8c50-3094c227569f', foundational, neither_sovereignty_nor_autonomy_holds_absolute_trump).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_holds_absolute_trump, holdable).
narrative_ontology:cs_axiom_grounding('59927ac0-73cf-41bb-8c50-3094c227569f', neither_sovereignty_nor_autonomy_holds_absolute_trump, conventional).
narrative_ontology:cs_axiom('59927ac0-73cf-41bb-8c50-3094c227569f', foundational, boundary_disputes_resolved_by_political_accommodation_not_legal_supremacy).
narrative_ontology:cs_axiom_status(boundary_disputes_resolved_by_political_accommodation_not_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('59927ac0-73cf-41bb-8c50-3094c227569f', boundary_disputes_resolved_by_political_accommodation_not_legal_supremacy, instrumental).
narrative_ontology:cs_reference_frame('59927ac0-73cf-41bb-8c50-3094c227569f', id_1997_joint_declaration_fifty_year_bargain).
narrative_ontology:cs_drift_state('59927ac0-73cf-41bb-8c50-3094c227569f', post_2020_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59927ac0-73cf-41bb-8c50-3094c227569f', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, international_investors_using_hk_gateway).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_independent_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_press_and_civil_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate sovereign authority over Hong Kong and can interpret, amend, or override the Basic Law through the National People's Congress Standing Committee. Negotiates the boundary of autonomy case by case — sometimes deferring to Hong Kong's separate legal and economic system when it serves stability and international confidence, sometimes intervening directly when framed as national security. Bears essentially no structural cost from the arrangement; can recalibrate the boundary unilaterally when it judges the accommodation has failed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, beijing_central_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, beijing_central_authorities, beneficiary).

% Administers the local legal, financial, and civil service systems under delegated authority, mediating between Beijing's directives and local expectations of autonomy. Must implement accommodations it did not fully author and absorb legitimacy costs when the boundary shifts against local sentiment; cannot exit the arrangement or unilaterally renegotiate its terms with Beijing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government_administration, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government_administration, payer).

% Benefits from Hong Kong's separate common-law commercial system, currency, and international financial connectivity that the 'two systems' half of the framework preserves. Retains capital mobility and can relocate operations or assets if the accommodation collapses, giving this group outsized leverage to lobby both sides toward continued functional coexistence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite, beneficiary,
    organized, biographical, mobile, global).

% Uses Hong Kong as a legal and financial bridge between mainland and global markets, benefiting from the separate judiciary and currency regime. Can withdraw capital or route transactions elsewhere at low cost, which gives this seat real bargaining power over how far Beijing pushes the sovereignty side of the negotiation before risking the economic function the arrangement exists to preserve.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_investors_using_hk_gateway, beneficiary,
    powerful, biographical, arbitrage, global).

% Sought to use the autonomy half of the arrangement to expand electoral and civil rights; found the boundary redrawn against them through national security legislation and disqualification mechanisms. Bears the direct cost of the negotiated boundary's contested edge — prosecution, exile, or political exclusion — with no capital or institutional leverage to shift where the line is drawn.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_pro_democracy_activists, payer,
    powerless, biographical, trapped, local).

% Operates the common-law system the accommodation is supposed to preserve, but faces recurring pressure when its rulings on politically sensitive cases are set aside or preempted by NPCSC interpretation. Retains genuine adjudicative function in commercial and civil matters, which is the visible evidence the coexistence reading points to, while its authority in constitutionally contested cases has narrowed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_independent_judiciary, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_independent_judiciary, observer).

% Operated under the autonomy guarantee's press-freedom provisions; increasingly constrained by security legislation applied to publication and assembly. Individual journalists and organizations that cannot relocate capital or operations bear the accommodation's tightening edge directly, with emigration as the only exit and that exit itself contested.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_press_and_civil_society, payer,
    powerless, biographical, trapped, local).

% Co-signed the Sino-British Joint Declaration and retain a diplomatic interest in the boundary's stability but have no enforcement mechanism inside the negotiated accommodation itself. Can impose sanctions, issue statements, or adjust trade posture, but are structurally outside the room where the sovereignty-autonomy boundary is actually redrawn.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_governments_and_treaty_parties, excluded,
    powerful, generational, analytical, global).

% Studies the framework as a case of functional federalism/asymmetric autonomy under contested sovereignty, comparing it to other hybrid arrangements. Has no stake in the outcome but documents how the boundary has moved and whether accommodation or unilateral revision predominates over time.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, constitutional_scholars_and_comparative_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves the problem of integrating a distinct legal, economic, and social system into a sovereign state without immediate homogenization — allowing Hong Kong's common-law commercial infrastructure to keep functioning as a bridge to global capital while nominal sovereignty is unified under Beijing.
% TRANSFER_FUNCTION: Moves political discretion over where the autonomy/sovereignty boundary sits from a fixed treaty guarantee toward case-by-case central accommodation; moves civil and political liberties from guaranteed status toward negotiated, revisable status, while preserving commercial and currency continuity for capital-holders.
% ABSENT_VOICES: The British government, as co-signatory to the Joint Declaration, has no seat inside the actual negotiation of the boundary once sovereignty transferred, and Hong Kong residents without capital mobility (most residents) had no direct voice in how the boundary would be renegotiated after 1997 — international governments and civil society groups can protest but cannot participate in the accommodation itself.
% DISAPPEARANCE_RATIONALE: If the framework's functional-division structure disappeared overnight and full unified sovereignty applied without any distinct system, Hong Kong's separate judiciary, currency, and common-law commercial system would collapse into the mainland system — a major rearrangement for capital and legal practice. But pro-democracy and civil-society stakeholders would argue the substantive protections already eroded far enough that the formal disappearance would change less than the framework's continued existence suggests; hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Reunifying a territory with a fundamentally different legal, economic, and political system into a single sovereign state without destroying the economic function that made the territory valuable, and without immediate confrontation with residents or international treaty partners who expected continuity.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong administration attest the founding problem — integrating a distinct system without destabilizing it — remains live and is being actively managed. Independent constitutional scholars and international treaty parties (outside both the Beijing and the Hong Kong civil-society camps) attest that the original 50-year negotiated-boundary problem has been substantially resolved in favor of the sovereignty side well before the 2047 horizon, with accommodation increasingly asymmetric rather than substantive negotiation.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, contested).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a medium 0.48 (not high) because this reading holds that a genuine coordination function survives — Hong Kong's distinct commercial and legal system continues to operate and continues to benefit capital-holders and Beijing alike, which is real coordination value, not merely cover. Suppression is authored lower (0.42) than a sovereignty-primacy reading would author it, because this reading holds that civil society and capital-holders retain real bargaining power (economic leverage, international attention) that constrains how far the boundary can move without triggering costly capital flight or international response — suppression here is bounded contestation, not unconstrained control. Theater ratio is moderate-low (0.28): the negotiated boundary produces real functional division (distinct currency, distinct judiciary in most matters) alongside a growing performative layer around 'high degree of autonomy' language that increasingly diverges from the tightening political-liberties boundary.
 *
 * PERSPECTIVAL GAP:
 *   Beijing and the business/investor seats will compute this constraint closer to rope or unremarkable functional coexistence — their situation includes bargaining power and functional benefit. The payer seats (activists, press, civil society) will compute it closer to snare — trapped exit, no leverage, direct cost from boundary movement. This divergence is exactly the seat-level split this reading predicts and the engine should register, rather than a defect requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing sits closest to the beneficiary end: it sets the terms of accommodation and bears essentially no structural cost from recalibrating the boundary. The Hong Kong administration sits mixed — it benefits from continued relevance and function but pays legitimacy costs it did not choose. Business elites and international investors sit near the beneficiary end because their exit options (capital mobility, arbitrage) both benefit them directly and generate the leverage that, under this reading, keeps the negotiation genuinely two-sided rather than purely dictated. Pro-democracy activists, press, and civil society sit at the target end: trapped exit options, no capital leverage, and they absorb the boundary's contested edge as it moves. The judiciary is intermediate — genuine function preserved in most matters, narrowed authority in the politically contested zone, which is precisely the site of negotiation this reading is about.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating a structurally distinct territory without destroying its economic function or provoking immediate confrontation — is authored as contested rather than flatly dead, because this reading holds that real negotiation (not mere theater) continues over the boundary: Beijing has in fact preserved substantial functional autonomy in commercial and legal matters even as it has tightened political-liberties boundaries. This prevents mislabeling the entire arrangement as pure extraction (which would erase the real coordination value capital-holders and even ordinary residents derive from the surviving common-law commercial system) while also refusing to certify it as pure coordination (which would erase the real, rising cost borne by activists, press, and the judiciary's constitutional jurisdiction). The tangled_rope classification is the structural expression of holding both facts simultaneously under this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_vs_unilateral_override,
    'Are the post-2019 boundary shifts (National Security Law, electoral reform, disqualifications) genuine bilateral accommodations responding to circumstances (e.g., the 2019 protests), or unilateral sovereign overrides dressed in accommodation language?',
    'Track whether Hong Kong-side actors (business elite, legal profession, civil society representatives) had any documented input into the specific terms of each boundary-shifting measure, versus measures being drafted and imposed by NPCSC/mainland bodies with no local negotiation channel.',
    'If the shifts were substantively negotiated with local stakeholders retaining some veto or shaping power, the balanced-coexistence reading holds and tangled_rope is the accurate classification. If the shifts were unilaterally imposed with accommodation language used only rhetorically, the constraint''s actual operation has converged on the sovereignty_primacy_reading regardless of this story''s claimed_type — which is precisely the divergence the corpus is built to detect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_vs_unilateral_override, empirical, 'Whether recent boundary shifts were genuinely negotiated or unilaterally imposed under accommodation rhetoric.').

omega_variable(
    capital_leverage_durability,
    'Does the bargaining power this reading attributes to business elites and international investors (capital mobility as a check on how far Beijing can move the boundary) remain real, or has it eroded as Hong Kong''s unique gateway function is increasingly substitutable by Shanghai, Singapore, or direct mainland market access?',
    'Track capital flow and financial-center relocation data over the interval; a durable leverage claim implies Beijing bears real cost from further tightening, while a substitutable-gateway finding implies the leverage this reading relies on to distinguish itself from sovereignty_primacy is weakening.',
    'If Hong Kong''s gateway function is substitutable, the bargaining power this reading attributes to capital-holders becomes illusory, undermining the structural basis for classifying this as a negotiated tangled_rope rather than a constraint drifting toward the sovereignty_primacy reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_leverage_durability, empirical, 'Whether capital mobility still constrains the boundary or has become a diminishing check as substitute financial centers grow.').

omega_variable(
    reading_choice_under_determination,
    'Is the choice to read this arrangement as ''negotiated accommodation'' rather than ''sovereignty override with cosmetic autonomy language'' itself contestable on the same facts?',
    'This is a conceptual/framing question inherent to kernel decomposition: compare this story''s ε (0.48, medium) against the sovereignty_primacy sibling''s ε (expected higher) and the autonomy_primacy sibling''s ε (expected lower, if any) authored independently — the size and direction of the gap is itself evidence about how much genuine interpretive latitude the underlying facts support.',
    'A narrow ε-gap across the three sibling readings would suggest the ''balanced coexistence'' framing is doing real interpretive work distinguishing a genuinely intermediate case; a wide gap with this story''s ε sitting far from both siblings would suggest one of the readings is empirically dominant and the others are advocacy positions rather than equally defensible structural readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_under_determination, conceptual, 'Whether the three kernel readings represent genuinely comparable interpretive latitude or one reading is empirically dominant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(one__tr_t14, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(one__tr_t17, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 17, 0.24).
narrative_ontology:measurement(one__tr_t22, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 22, 0.26).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(one__tr_t28, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 28, 0.28).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(one__be_t14, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 14, 0.36).
narrative_ontology:measurement(one__be_t17, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 17, 0.44).
narrative_ontology:measurement(one__be_t22, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 22, 0.46).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(one__be_t28, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 28, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(one__su_t14, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 14, 0.27).
narrative_ontology:measurement(one__su_t17, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 17, 0.38).
narrative_ontology:measurement(one__su_t22, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 22, 0.4).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(one__su_t28, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 28, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__balanced_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_national_security_law_enforcement).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judicial_independence_erosion).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the one_country_two_systems_framework kernel, decomposed per the ε-invariance principle because the natural-language label 'One Country, Two Systems' covers structurally distinct claims about where sovereignty and autonomy sit relative to each other. sovereignty_primacy_reading authors autonomy as delegated and revocable (expected higher ε, closer to snare/tangled_rope with fewer genuine coordination protections). autonomy_primacy_reading authors autonomy as treaty-guaranteed and internationally enforceable (expected lower ε, closer to rope/tangled_rope with stronger coordination weighting). This balanced_coexistence_reading sits structurally between them, authoring genuine bilateral negotiation with contested, moving boundaries — medium ε, tangled_rope, with real bargaining power on both sides. All three share the same underlying kernel (the Basic Law / Joint Declaration framework) but instantiate different constraints because their beneficiary/victim structures, enforcement postures, and exit-option assessments for the same nominal stakeholders differ by reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
