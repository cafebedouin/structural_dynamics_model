% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Satisfaction Substrate (Cultural Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The honor satisfaction substrate — the cultural infrastructure that made
 *   dueling a thinkable, legitimate, even obligatory response to honor
 *   insults — underwent foundational transformation between 1750 and 1900 in
 *   Western societies. As 'cultures of honor' (where status derives from
 *   willingness to risk violence for reputation) gave way to 'cultures of
 *   dignity' (where status derives from intrinsic worth and legal rights),
 *   the substrate itself disintegrated. Dueling did not merely become
 *   illegal; it became unthinkable. The constraint is mountain erosion: the
 *   physical/social substrate supporting the practice collapsed from within,
 *   not through external suppression alone. The honor code's natural-law
 *   presentation (emerges_naturally=true) masks its function as elite status
 *   regulation, making this a false_summit_mountain candidate.
 *
 * KEY AGENTS:
 *   - traditional_aristocracy: Primary beneficiary (institutional/identity_locked) — honor code legitimated their status and provided exclusive satisfaction mechanism
 *   - military_officer_corps: Primary beneficiary (organized/identity_locked) — officer honor culture structured professional identity and conflict resolution
 *   - landed_gentry: Beneficiary (moderate/constrained) — local elite status regulated through honor performances
 *   - bourgeois_professionals: Emergent beneficiary (organized/mobile) — dignity culture aligned with professional reputation and legal standing
 *   - state_legal_authorities: Agenda_setter (institutional/analytical) — imposed monopoly on violence but cultural contraction preceded legal enforcement
 *   - common_population: Excluded (powerless/trapped) — honor code never extended full satisfaction rights to them; dignity culture eventually included them
 *   - historical_sociologists: Observer (analytical/analytical) — analyze the transformation from outside the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.15).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.25).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Satisfaction Substrate (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '19a4e225-cd9b-45c1-a6b8-1659293696c3').
narrative_ontology:cs_kernel_codification('19a4e225-cd9b-45c1-a6b8-1659293696c3', distributed).
narrative_ontology:cs_authority_grounding('19a4e225-cd9b-45c1-a6b8-1659293696c3', practice).
narrative_ontology:cs_reading_relation('19a4e225-cd9b-45c1-a6b8-1659293696c3', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('19a4e225-cd9b-45c1-a6b8-1659293696c3', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('19a4e225-cd9b-45c1-a6b8-1659293696c3', foundational, honor_code_endogenous_transformation).
narrative_ontology:cs_axiom_status(honor_code_endogenous_transformation, holdable).
narrative_ontology:cs_axiom_grounding('19a4e225-cd9b-45c1-a6b8-1659293696c3', honor_code_endogenous_transformation, empirically_contingent).
narrative_ontology:cs_axiom('19a4e225-cd9b-45c1-a6b8-1659293696c3', foundational, dueling_unthinkability_via_dignity_culture).
narrative_ontology:cs_axiom_status(dueling_unthinkability_via_dignity_culture, holdable).
narrative_ontology:cs_axiom_grounding('19a4e225-cd9b-45c1-a6b8-1659293696c3', dueling_unthinkability_via_dignity_culture, empirically_contingent).
narrative_ontology:cs_reference_frame('19a4e225-cd9b-45c1-a6b8-1659293696c3', culture_of_honor_substrate).
narrative_ontology:cs_drift_state('19a4e225-cd9b-45c1-a6b8-1659293696c3', culture_of_dignity_ascendancy, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('19a4e225-cd9b-45c1-a6b8-1659293696c3', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, traditional_aristocracy).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, landed_gentry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, bourgeois_professionals).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, bourgeois_professionals).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, honor_as_natural_social_order).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, violence_as_legitimate_status_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary elites whose status claims were legitimated by the honor code. The substrate provided exclusive access to honor satisfaction (dueling) which both regulated intra-elite conflict and signaled superiority over non-elites. As dignity culture spread, their identity frame — 'we are the ones who duel for honor' — became incoherent. Exit meant abandoning the core of aristocratic self-conception; many instead performed honor theatrically (non-lethal duels, ritualized insults) while losing real regulatory function.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, traditional_aristocracy, beneficiary,
    institutional, generational, identity_locked, continental).

% Officer corps across European and American militaries maintained honor culture as professional identity well into the 19th century. Duelling regulated internal hierarchy and civilian-military boundaries. The substrate's collapse forced a painful identity transition: from 'men of honor who settle disputes with pistols' to 'professionals who submit to military law and civilian courts.' Some regiments maintained dueling cultures theatrically past 1900, but the substrate's intelligibility had eroded — younger officers no longer 'felt' the obligation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps, beneficiary,
    organized, biographical, identity_locked, national).

% Local elites who used honor performances to maintain social dominance in rural and small-town settings. They had more exit options than high aristocracy or career officers — could transition to dignity-culture norms (legal reputation, civic leadership) without total identity loss. Many became early adopters of dignity culture, using legal courts to settle disputes that their fathers would have dueled over. Their constrained exit reflects economic dependence on local reputation networks.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, landed_gentry, beneficiary,
    moderate, biographical, constrained, regional).

% Lawyers, doctors, merchants, and civil servants whose status derived from professional reputation and legal standing rather than birth. They were the primary carriers of dignity culture. The honor substrate initially obstructed them (aristocrats refused to duel commoners, denying them satisfaction), making them payers in the early period. As dignity culture rose, they became beneficiaries of the new substrate. Their mobility allowed them to navigate the transition strategically — adopting honor performances when useful, dignity norms when advantageous.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, bourgeois_professionals, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, bourgeois_professionals, payer).

% Courts, police, and legislatures that progressively criminalized dueling (e.g., France 1626 edict reinforced 1790s; Prussia 1850s; US state laws 1830s-1880s). In this reading, legal suppression followed rather than caused cultural contraction — laws caught up to a transformation already underway. Authorities benefited from the monopoly on legitimate violence but did not drive the cultural shift. Their analytical seat sees the full structural picture: honor substrate eroding, dignity substrate crystallizing, legal framework adapting.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% The vast majority excluded from honor satisfaction entirely — honor code applied only to 'gentlemen.' They bore the costs of elite violence (duels in public spaces, culture of impunity) without access to its benefits. Dignity culture eventually included them via legal rights and intrinsic worth claims, but the transition was slow and uneven. They had no voice in either honor or early dignity cultures; their 'exit' from honor subjection came only through state law and social movements, not cultural transformation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, common_population, excluded,
    powerless, immediate, trapped, local).

% Analysts (Weber, Elias, Pinker, Nisbett, Cohen) who study the honor→dignity transition as a macro-cultural shift. They see the substrate's disintegration as a civilizational-scale rearrangement: from violence-based status regulation to law-based rights regulation. Their analytical seat is outside the constraint's operation — they neither benefit nor pay — but their framing shapes how the constraint's legacy is understood in contemporary discourse.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a thinkable, legitimate action-set for honor satisfaction among status-equals: dueling regulated elite conflict, signaled commitment to reputation, and maintained status boundaries without state intervention. The substrate made honor demands intelligible and actionable — a coordination mechanism for a violence-based status economy.
% TRANSFER_FUNCTION: Moved status, risk, and social capital through ritualized violence: the challenger risked death to prove honor; the challenged risked death to avoid dishonor; spectators transferred deference to the victor. The substrate transferred regulatory authority from state to peers within the honor group. As dignity culture rose, this transfer reversed: status regulation moved to legal/reputational mechanisms, risk moved to professional/financial domains.
% ABSENT_VOICES: Women (excluded from honor satisfaction entirely, subject to male honor violence without recourse), enslaved and colonized peoples (honor code explicitly denied them standing), urban poor and industrial workers (honor culture never extended to them; they would have objected to both dueling's public danger and its class exclusivity). These voices were structurally absent from the honor substrate; their inclusion came only with dignity culture's universalist claims.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction substrate vanished overnight in 1800, elite conflict regulation would have collapsed into either state law (prematurely, without cultural legitimacy) or chaotic violence. The substrate's gradual erosion allowed dignity culture to crystallize as a replacement coordination mechanism. By 1900, its disappearance would rearrange little — dignity culture and state law had already absorbed its regulatory function. The verdict is contested across the interval: early = world_rearranges; late = world_unchanged.
% FOUNDING_PROBLEM: Regulating elite conflict and status claims in societies where the state lacked a monopoly on legitimate violence and where birth-based hierarchy required continuous performative validation. The honor substrate solved this by making violence a regulated, peer-enforced status currency.
% FOUNDING_PROBLEM_CORROBORATION: Norbert Elias (The Civilizing Process), Randall Collins (Violence: A Micro-sociological Theory), and Steven Pinker (The Better Angels of Our Nature) — all outside the benefiting aristocratic/military classes — document the state's violence monopoly and cultural pacification as historical facts. The benefiting classes (aristocracy, officer corps) contested the problem's death, claiming honor remained necessary for 'true' status regulation; their self-assertion is the cover story the mandatrophy analysis detects.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the honor substrate primarily coordinated status regulation among elites rather than extracting resources. Suppression is low (0.25) because the constraint's persistence did not depend on active coercion — it depended on shared cultural intelligibility. Theater_ratio rises to 0.65 because as dignity culture spread, honor performances became increasingly performative (duels fought with deliberately non-lethal intent, ritualized apologies replacing blood). Accessibility_collapse is very high (0.92) because once the dignity-culture frame took hold, the honor frame became cognitively inaccessible — dueling literally became unthinkable, not just illegal. Resistance is near zero (0.08) because the transformation was endogenous; elites themselves adopted dignity norms. The metrics describe a mountain eroding: high naturalness appearance, near-zero resistance, collapsing alternatives, rising theater as the substrate hollows out.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional_aristocracy and military_officer_corps seats (identity_locked, institutional power), the honor substrate appeared as mountain — natural, inevitable, the only way to regulate honor. From the bourgeois_professionals seat (mobile, organized), the same substrate appeared as an archaic obstruction to meritocratic dignity. From the state_legal_authorities seat (institutional, analytical), it appeared as a rival sovereignty claim to be suppressed. The engine computes this divergence: the beneficiary seats experience mountain; the emerging professional seat experiences snare (obstruction); the state seat experiences tangled_rope (coordination of violence monopoly with extraction of honor's regulatory function).
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional elites and military officers are beneficiaries (d near 0.0) — the substrate subsidized their status claims and provided exclusive conflict resolution. They are identity_locked: their professional and social identity was constituted through honor culture; exit meant identity dissolution. Bourgeois professionals are mobile (d ~ 0.4) — they could navigate both honor and dignity frames during transition, and dignity culture offered them superior status recognition. Common population is trapped (d ~ 0.7) — excluded from honor satisfaction entirely, subject to elite violence without recourse. State authorities are analytical (d = 0.5) — they neither benefit nor pay in the honor economy but seek to monopolize violence regulation. The cultural contraction reading emphasizes that the beneficiaries' identity_lock made them unable to perceive the substrate's constructedness until dignity culture offered an alternative identity frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The honor substrate's founding problem — regulating elite conflict without state monopoly on violence — was live in 1750 but dead by 1900 (state monopoly established, dignity culture supersedes). The arrangement persisted theatrically (rising theater_ratio) after its founding problem died, a classic mandatrophy pattern. However, this reading argues the substrate did not merely persist vacuously; it actively disintegrated (mountain erosion). The mandatrophy is resolved not by abolition but by cultural mutation: the substrate transformed into dignity culture, carrying forward the coordination function (status regulation) without the extraction mechanism (ritualized violence). This distinguishes cultural_contraction_reading from practice_decline_reading (which sees mere exogenous suppression) and composite_overdetermined_reading (which sees both).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_privilege,
    'Was the honor code a genuine natural law of social coordination, or a constructed constraint that benefited identifiable elites by making their status claims appear inevitable?',
    'Comparative historical analysis of honor systems across cultures: if honor codes vary systematically with elite power structures, the natural-law claim is undermined; if they converge on universal features independent of elite interests, natural-law gains support.',
    'If constructed, the mountain classification collapses to false_summit_mountain (tangled_rope override); the cultural contraction is revealed as elite privilege losing its mask. If natural, the erosion is a genuine cultural loss with no extraction story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_privilege, conceptual, 'Natural-law status of honor code vs. elite-serving construction').

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is the cultural_contraction_reading of kernel honor_satisfaction_substrate. How does this reading''s structural delta (honor code collapses as substrate; dueling exits thinkable action-set endogenously) differ from sibling readings?',
    'Structural comparison of the three readings'' causal claims: practice_decline_reading attributes dueling''s end to exogenous suppression with persistent honor code; composite_overdetermined_reading claims simultaneous exogenous and endogenous pathways. This reading asserts endogenous transformation as primary and sufficient.',
    'If cultural contraction is the dominant pathway, the constraint''s erosion is mountain-like (substrate disintegration) not suppression-driven. This determines whether the constraint''s terminal state is mountain→piton (erosion) or mountain→snare (suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Committer frame: this reading''s structural identity within the honor_satisfaction_substrate kernel').

omega_variable(
    endogenous_vs_exogenous_causal_weight,
    'What is the relative causal weight of endogenous cultural transformation (honor→dignity) vs. exogenous legal/institutional suppression in removing dueling from the thinkable action-set?',
    'Counterfactual historical modeling: in jurisdictions where legal suppression was weak/late but dignity culture spread (e.g., American South vs. North, or European peripheries), did dueling decline on the same timeline? If yes, endogenous weight increases.',
    'High endogenous weight validates this reading''s mountain-erosion claim (substrate disintegrates from within). High exogenous weight validates practice_decline_reading or composite_overdetermined_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causal_weight, empirical, 'Causal weighting between cultural transformation and legal suppression in dueling''s extinction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.25) structural (legal prohibitions, state monopoly on violence) or internalized (honor culture''s own transformation making dueling unthinkable)?',
    'Post-erosion suppression trajectory: if suppression persists after honor culture dissolves (e.g., anti-dueling laws enforced against residual practitioners), structural component dominates. If suppression vanishes with the culture, it was internalized.',
    'If internalized, the constraint''s effective suppression is higher during transition than the structural measure suggests — the target carries the suppression with them via transformed habitus. This affects whether the constraint classifies as mountain (low suppression) or piton (performative maintenance of dead code).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in honor culture transformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1780, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1780, 0.15).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1810, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1810, 0.25).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1840, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1840, 0.4).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1870, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1870, 0.55).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.65).

% Extraction over time
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.05).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1780, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1780, 0.06).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1810, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1810, 0.08).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1840, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1870, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1870, 0.12).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1780, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1780, 0.12).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1810, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1810, 0.15).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1840, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1840, 0.2).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1870, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1870, 0.23).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% Kernel honor_satisfaction_substrate decomposes into three readings differing on causal pathway: cultural_contraction_reading (endogenous substrate collapse, mountain erosion), practice_decline_reading (exogenous suppression, persistent substrate), composite_overdetermined_reading (simultaneous non-independent pathways). This reading's ε (0.15) reflects low extraction because the transformation is cultural, not extractive; sibling readings would author different ε values for the same referent period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, institutional, 0.1).
constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, organized, 0.15).
constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, moderate, 0.4).
constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
