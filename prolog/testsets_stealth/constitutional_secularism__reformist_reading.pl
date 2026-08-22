% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Secularism: Affirmative State Duty to Eliminate Oppressive Religious Practice
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the constitutional_secularism
 *   kernel: the reformist reading, under which the state bears an affirmative
 *   duty to eliminate religious practices that oppress marginalized groups,
 *   and religious-autonomy claims yield to that duty. The standing
 *   arrangement under contest is the operative intervention machinery: courts
 *   adjudicating which practices are integral and which regulable,
 *   legislatures codifying entry and anti-untouchability mandates, police
 *   enforcing access orders. Scheduled castes and women barred from worship
 *   receive access and statutory levers; orthodox institutions, hereditary
 *   priesthoods, conservative practitioner communities, and minority-faith
 *   institutions lose ritual self-governance and fund decades of defensive
 *   litigation. The claim and the metrics are independent authored facts: the
 *   constraint is CLAIMED as tangled_rope from its structure (real protection
 *   function + asymmetric extraction + active enforcement), while the metrics
 *   describe its actual operation, including a rising extraction trajectory
 *   the engine evaluates independently.
 *
 * KEY AGENTS:
 *   - reformist_judiciary: agenda setter (institutional/constrained) — administers the duty; each intervention converts regulated autonomy into precedent authority the bench itself holds
 *   - scheduled_castes_and_tribes: primary beneficiary (organized/trapped) — receives entry mandates and anti-untouchability enforcement; cannot exit the structure being reformed
 *   - women_denied_temple_entry: primary beneficiary (organized/constrained) — receives access rulings while remaining inside the communities whose rules change
 *   - orthodox_religious_institutions: primary target (powerful/identity_locked) — loses ritual governance, funds litigation; exit equals dissolution
 *   - conservative_practitioner_communities: target (moderate/identity_locked) — bears restructured daily practice across communities
 *   - hereditary_priestly_lineages: target (moderate/identity_locked) — loses ritual monopoly and hereditary succession
 *   - minority_faith_institutions: target (powerful/constrained) — policed, by their account, through a majoritarian filter
 *   - conservative_women_devotees: excluded voice (organized/identity_locked) — members of the beneficiary class who oppose the specific intervention and are spoken for without consent
 *   - constitutional_scholars: analytical observer (analytical/analytical) — records and theorizes the doctrine's arc
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.66).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.72).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Secularism: Affirmative State Duty to Eliminate Oppressive Religious Practice").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '69958c64-d55a-4591-be09-a07a495205bb').
narrative_ontology:cs_kernel_codification('69958c64-d55a-4591-be09-a07a495205bb', formalized).
narrative_ontology:cs_authority_grounding('69958c64-d55a-4591-be09-a07a495205bb', lineage).
narrative_ontology:cs_interpretation_layer_present('69958c64-d55a-4591-be09-a07a495205bb').
narrative_ontology:cs_reading_relation('69958c64-d55a-4591-be09-a07a495205bb', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('69958c64-d55a-4591-be09-a07a495205bb', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('69958c64-d55a-4591-be09-a07a495205bb', foundational, antihierarchy_duty_supersedes_religious_autonomy).
narrative_ontology:cs_axiom_status(antihierarchy_duty_supersedes_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('69958c64-d55a-4591-be09-a07a495205bb', antihierarchy_duty_supersedes_religious_autonomy, deontological).
narrative_ontology:cs_axiom('69958c64-d55a-4591-be09-a07a495205bb', secondary, constitutional_morality_over_community_sentiment).
narrative_ontology:cs_axiom_status(constitutional_morality_over_community_sentiment, holdable).
narrative_ontology:cs_axiom_grounding('69958c64-d55a-4591-be09-a07a495205bb', constitutional_morality_over_community_sentiment, deontological).
narrative_ontology:cs_reference_frame('69958c64-d55a-4591-be09-a07a495205bb', transformative_social_revolution_mandate).
narrative_ontology:cs_drift_state('69958c64-d55a-4591-be09-a07a495205bb', contemporary_expansive_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('69958c64-d55a-4591-be09-a07a495205bb', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes_and_tribes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_denied_temple_entry).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, orthodox_religious_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, conservative_practitioner_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, hereditary_priestly_lineages).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, minority_faith_institutions).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, transformative_constitutionalism).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, constitutional_morality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Supreme Court and high courts administer the duty: they hear challenges to exclusionary practices, decide which practices are integral to a faith and which are regulable, and order entry, admission, or cessation. Each ruling adds to a body of precedent the bench itself controls and expands the doctrinal toolkit available for the next intervention. Its costs are institutional criticism, review petitions, and accusations of overreach — not the regulated practices themselves. Precedent binds it in both directions; it cannot simply stop deciding religion cases.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, reformist_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Dalit and Adivasi communities were the paradigm targets of exclusionary practice — denied temple entry, segregated in village religious life, confined to hereditary degraded labor justified as religious obligation. The duty hands them statutory and constitutional levers: entry mandates, anti-untouchability enforcement, atrocity-law protection. Their exit from the caste-religious structure is costly in every direction — conversion forfeits scheduled-caste benefits and invites social sanction — so relief has to arrive inside the structure they cannot leave.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes_and_tribes, beneficiary,
    organized, generational, trapped, national).

% Women barred from particular shrines or forms of worship — menstruation-age bans, mosque entry restrictions — gain court-backed access claims. They remain members of the very faith communities whose rules are being changed; exit means abandoning family and devotional life, so most seek reform from within. Organized women's groups litigate and campaign; individual devotees mostly comply with whichever rule currently stands.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_denied_temple_entry, beneficiary,
    organized, biographical, constrained, national).

% Mathas, mutts, temple trusts, and denominational boards administer the practices the duty reaches. They lose control over ritual calendars, entry rules, and internal discipline when courts override them, and they fund decades of litigation defending their self-governance. Their existence is constituted by the tradition being regulated — dissolving into a externally administered compliant practice is, from their seat, a form of ending. Wealth and social standing give them staying power, not exit.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, orthodox_religious_institutions, payer,
    powerful, civilizational, identity_locked, national).

% Ordinary believers across Hindu, Muslim, and Christian communities whose customary practices are restructured by external mandate — entry opened, personal law rewritten, dress codes adjudicated. They bear the daily cost of changed practice and the experience of a tradition governed from outside. Leaving the community is unthinkable at the scale of family, marriage, and burial; compliance is the realistic response.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, conservative_practitioner_communities, payer,
    moderate, biographical, identity_locked, national).

% Priest families whose ritual authority and livelihood rest on exclusive control of sancta. Entry mandates and state-administered temple governance dilute their office; some face training and certification requirements that break hereditary succession. Their skills are specific to the office being regulated; exit means abandoning a vocation that is also a birthright.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, hereditary_priestly_lineages, payer,
    moderate, generational, identity_locked, regional).

% Muslim and Christian institutions subject to the same duty — personal-law codification, entry rulings, dress-code litigation. Many argue the duty reaches them through a majoritarian filter: their practices are policed sooner while majority-community practices receive longer deference. They litigate defensively and lack the numerical political weight of majority-community institutions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, minority_faith_institutions, payer,
    powerful, generational, constrained, national).

% Women devotees who supported the entry restrictions — in the major shrine-entry controversies, large numbers of women protested in favor of preserving the custom. The duty speaks for 'women' as a class; these women experience being spoken for without their consent. They appear in litigation mainly as respondents or counter-petitioners, not as authors of the settlement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, conservative_women_devotees, excluded,
    organized, biographical, identity_locked, national).

% Comparative constitutionalists and legal academics track the doctrine's arc, publish critiques and defenses, and supply the vocabulary in which the duty's limits are argued. They bear none of the constraint's costs and collect none of its benefits; their seat is the record-keeping one.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, reformist_judiciary).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem internal to communities: members subordinated by religious hierarchy cannot renegotiate the rules that bind them, because exit is costly and internal voice is suppressed; a single external standard of constitutional equality, applied across all communities, prevents a race toward competitive orthodoxy and gives internal reformers a lever they cannot manufacture alone.
% TRANSFER_FUNCTION: Moves ritual self-governance and definitional authority over religious practice from religious institutions and practitioner majorities to state institutions (courts, legislatures, enforcement agencies), and moves access, status, and legal protection to members of marginalized groups inside those communities.
% ABSENT_VOICES: Conservative members of the beneficiary classes themselves — women who defended the entry bans, dalits who find meaning in the tradition as it stands — would object that the duty presumes their preferences; they are present in the record only as litigants and protesters, never as co-authors of the settlement. Traditionalist laity generally appear only as defendants.
% DISAPPEARANCE_RATIONALE: If the duty vanished overnight, temple-entry regimes, anti-untouchability enforcement, and personal-law reform would lose their constitutional foundation; contested practices would revert to internal community control; beneficiary classes would lose levers they cannot replace internally; and the courts would shed a docket that structures their modern authority. The religious-legal order would reorganize around whatever autonomy settlements each community could impose internally.
% FOUNDING_PROBLEM: Communities structured by caste and gender hierarchy used religious autonomy as a shield for practices that subordinated members could not escape — untouchability, entry bans, exclusion from worship — and the constitutional framers judged that religious-freedom claims must yield where they protected such subordination.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: human-rights commission findings on manual scavenging and temple exclusion, sociological documentation of caste-segregated religious life, and testimony of internal reformers within the traditions themselves (reformist voices inside Hinduism, Muslim women's organizations). Religious institutions have conceded historical exclusion in parts of the litigation record. No corroborating source attests that the problem is solved.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66) is high but bounded: the duty reaches only practices the state classifies as oppressive, yet within that reach it overrides self-governance outright, and the reach has widened monotonically across the interval (essential-practices consolidation, then constitutional-morality rulings extending the duty to gender and dress across communities). Suppression (0.72) reflects criminal penalties, police-enforced entry, and contempt backing — substantial but targeted, not totalizing; suppression is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness is scaled, by the engine, through directionality and scope). Theater (0.33) is rising: landmark declaratory victories with patchy ground implementation (post-ruling entry rates far below headline rulings) mean a growing share of activity defends the doctrine symbolically rather than changing practice. Accessibility_collapse (0.5): once the duty applies, the prohibited practice cannot simply continue, but modification, reinterpretation, and private observance persist as partial alternatives. Resistance (0.65): sustained litigation, mass protest, and review petitions from identity-locked payers. The temporal series run on ONE shared grid (t=0,10,20,30,40,50,60,75) so every metric is authored at every examined point; the suppression_requirement series is authored because enforcement capacity is the traced dynamic — high early (criminal-law machinery for anti-untouchability enforcement), dipping mid-interval during the judicial-restraint phase, then ratcheting upward as courts took direct intervention roles. Receipt surface: gain_flow names reformist_judiciary because the extracted thing — autonomy and definitional authority over religious practice — demonstrably accrues to the bench as precedent and jurisdiction, distinct from the material benefits flowing to the beneficiary classes; fixing_cost is prohibitive because reversal requires constitutional amendment or wholesale doctrinal overruling against organized beneficiary classes and entrenched precedent, a cost exceeding any plausible benefit to a fixer.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the beneficiary seats (trapped/constrained exit), the arrangement is protection unavailable through any internal channel — the highest-value good they cannot otherwise obtain. From the payer seats (identity_locked exit), the same structure is expropriation of self-governance with no exit that preserves identity. From the agenda_setter seat, it is mandate fulfillment that also accretes institutional authority — which is precisely why the extraction trajectory rises: the seat that decides scope also collects from scope expansion. The excluded seat experiences misrepresentation: the duty claims to speak for a class whose members dispute it. The engine derives these per-seat classifications from the structural data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (scheduled castes and tribes, women denied entry) drive those seats toward the beneficiary end of directionality; their trapped/constrained exits push them further from arbitrage. Victim declarations (orthodox institutions, practitioner communities, priestly lineages, minority-faith institutions) drive those seats toward the full-target end, with identity_locked exits amplifying effective extraction — they cannot route around the duty without dissolving what they are. One override is declared: the institutional power atom (occupied in this story only by reformist_judiciary) is set to d=0.18 because the structural derivation, finding no beneficiary/victim declaration for the agenda_setter, would leave it near-symmetric; in fact the seat converts each intervention into precedent authority it holds, placing it near the beneficiary end. This is the ratchet mechanism behind the rising base_extractiveness series.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (hierarchy shielded by religious autonomy) is live and independently corroborated, so the mandate has not outlived its function. The classification work runs in both directions. Labeling this a rope would erase the accumulating extraction visible in the temporal series — scope widening past the founding problem toward identity management, with gains accruing to the deciding seat. Labeling it a snare would erase the genuine coordination function: trapped beneficiaries with no internal exit channel really do receive protection here, and the victim set is defined by opposition to that protection, not by the absence of one. Tangled_rope with a rising extraction trajectory marks it as a watch item: the same structure that protects can drift toward extraction if the oppression-boundary continues to track the boundary-drawer's authority rather than harm evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the affirmative-duty reading, rather than strict neutrality or permission-level intervention, the correct instantiation of the constitutional_secularism kernel?',
    'Cross-jurisdictional comparison of secularism designs tracking both marginalized-group welfare and the durability of religious-freedom protection; adoption patterns of courts and legislatures facing the same choice.',
    'Adopting strict_neutrality_reading would dissolve this constraint''s victim set entirely (non-interference extracts nothing from autonomy holders); adopting principled_intervention_reading would bound extraction at case-by-case necessity and flatten the ratchet. This story''s classification is valid only within the reformist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: this constraint is one reading of the constitutional_secularism kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    consent_of_the_marginalized,
    'Does the duty track the considered preferences of the marginalized groups it names, or does it substitute state and elite judgment for theirs — given that organized conservative women devotees mobilized in favor of the restrictions the duty removes?',
    'Deliberative-participation data within affected communities; comparison of outcomes where affected-class members author the reform versus where courts impose it; longitudinal preference surveys inside the beneficiary classes.',
    'If the duty predominantly imposes outsider preferences, the beneficiary declaration overstates the coordination function and the arrangement drifts toward pure imposition — the computed classification moves toward the snare side despite the declared beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_of_the_marginalized, empirical, 'Whether the protection function serves the protected class''s own settled will or replaces it.').

omega_variable(
    oppression_boundary_drawing,
    'Who draws the line between ''practice oppressing marginalized groups'' (regulable) and ''core identity practice'' (protected), and does the line-drawer''s incentive contaminate the line?',
    'Audit of essential-practices and constitutional-morality rulings correlating doctrinal reach with the deciding institution''s jurisdictional expansion, controlling for harm evidence quality.',
    'If the boundary tracks authority growth rather than documented harm, the measured extraction understates the ratchet and the rising base_extractiveness series is steeper than authored; the agenda_setter seat''s effective extraction rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oppression_boundary_drawing, conceptual, 'Whether the regulable/protected boundary is drawn by harm evidence or by the boundary-drawer''s institutional interest.').

omega_variable(
    selective_application_bias,
    'Is the duty applied symmetrically across religious communities, or filtered through majoritarian politics such that minority-faith practices are policed sooner?',
    'Event-history analysis of intervention timing and outcome by community, controlling for the severity of the underlying exclusionary practice.',
    'Asymmetric application would concentrate extraction on minority_faith_institutions beyond their declared share, corrode the equal-standard coordination claim, and shift the victim-weighting the engine computes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_bias, empirical, 'Symmetry of the duty''s application across majority and minority communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__reformist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__reformist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__reformist_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__reformist_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(cons_tr_t60, observed).
narrative_ontology:measurement(cons_tr_t75, constitutional_secularism__reformist_reading, theater_ratio, 75, 0.33).
narrative_ontology:measurement_basis(cons_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__reformist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__reformist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__reformist_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__reformist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(cons_be_t60, observed).
narrative_ontology:measurement(cons_be_t75, constitutional_secularism__reformist_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement_basis(cons_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__reformist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__reformist_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__reformist_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__reformist_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement_basis(cons_su_t60, observed).
narrative_ontology:measurement(cons_su_t75, constitutional_secularism__reformist_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(cons_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, principled_intervention_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'constitutional secularism' (epsilon-invariance principle): the label covers three structurally distinct commitments with different epsilon values, beneficiary/victim sets, and failure modes. Upstream: strict_neutrality_reading (classical baseline, lowest extraction on autonomy holders). Middle: principled_intervention_reading (permissive intervention; moderate extraction). Downstream: this file, the reformist reading (affirmative duty; highest extraction on religious autonomy, widest beneficiary set). The upstream readings are cited as authority for the downstream one — intervention-permission precedent is the ladder by which the duty reading climbs — so edges run from each story to its dependents. Each member carries its own stable epsilon; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
