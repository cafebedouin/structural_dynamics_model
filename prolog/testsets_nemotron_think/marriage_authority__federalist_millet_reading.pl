% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Millet System: Fragmented Marriage Authority as Anti-Majoritarian Consociational Mechanism
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint story models the federalist_millet_reading of the
 *   marriage_authority kernel: a consociational arrangement where marriage
 *   authority is deliberately fragmented across community-specific personal
 *   law codes to prevent majoritarian domination. The fragmentation is not
 *   accidental — it is the mechanism. Legislative paralysis on family law
 *   reform is the intended operation, not a dysfunction. The reading claims
 *   this is a low-extraction rope (coordination mechanism) that benefits
 *   minority communities by protecting their jurisdictional autonomy. The
 *   same kernel supports four other readings (communal_autonomy, secularist,
 *   gender_rights, judicial_harmonization) that disagree on whether the
 *   fragmentation is protective, anachronistic, oppressive, or evolving.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.18).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.22).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Millet System: Fragmented Marriage Authority as Anti-Majoritarian Consociational Mechanism").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal_pluralism/constitutional_law/comparative_family_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '8f3482a4-e1bd-40a0-8534-bf7021e66708').
narrative_ontology:cs_kernel_codification('8f3482a4-e1bd-40a0-8534-bf7021e66708', formalized).
narrative_ontology:cs_authority_grounding('8f3482a4-e1bd-40a0-8534-bf7021e66708', lineage).
narrative_ontology:cs_interpretation_layer_present('8f3482a4-e1bd-40a0-8534-bf7021e66708').
narrative_ontology:cs_reading_relation('8f3482a4-e1bd-40a0-8534-bf7021e66708', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f3482a4-e1bd-40a0-8534-bf7021e66708', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('8f3482a4-e1bd-40a0-8534-bf7021e66708', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f3482a4-e1bd-40a0-8534-bf7021e66708', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('8f3482a4-e1bd-40a0-8534-bf7021e66708', foundational, fragmentation_prevents_tyranny).
narrative_ontology:cs_axiom_status(fragmentation_prevents_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('8f3482a4-e1bd-40a0-8534-bf7021e66708', fragmentation_prevents_tyranny, conventional).
narrative_ontology:cs_axiom('8f3482a4-e1bd-40a0-8534-bf7021e66708', foundational, elite_bargain_legitimates_pluralism).
narrative_ontology:cs_axiom_status(elite_bargain_legitimates_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('8f3482a4-e1bd-40a0-8534-bf7021e66708', elite_bargain_legitimates_pluralism, conventional).
narrative_ontology:cs_axiom('8f3482a4-e1bd-40a0-8534-bf7021e66708', secondary, legislative_paralysis_as_stability).
narrative_ontology:cs_axiom_status(legislative_paralysis_as_stability, holdable).
narrative_ontology:cs_axiom_grounding('8f3482a4-e1bd-40a0-8534-bf7021e66708', legislative_paralysis_as_stability, conventional).
narrative_ontology:cs_reference_frame('8f3482a4-e1bd-40a0-8534-bf7021e66708', consociational_compromise).
narrative_ontology:cs_drift_state('8f3482a4-e1bd-40a0-8534-bf7021e66708', post_constitutional_challenges_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8f3482a4-e1bd-40a0-8534-bf7021e66708', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, communal_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, majority_community).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_democracy_theory).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, anti_majoritarian_federalism).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, legal_pluralism_as_stability_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious and ethnic minority communities whose family law (marriage, divorce, inheritance) is governed by community-specific personal law codes rather than a uniform civil code. The fragmentation of marriage authority protects them from majoritarian legislative imposition. Community members cannot easily exit the personal law system without leaving the community itself — identity is fused with the legal framework. Community elites negotiate with the state to maintain this autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    organized, generational, identity_locked, national).

% Recognized leaders of minority communities (religious authorities, elected community bodies) who administer personal law codes and negotiate their scope with the state. They benefit from the fragmented authority — it secures their jurisdictional monopoly over family matters within the community. Their exit options are constrained: they operate within the consociational bargain and cannot unilaterally abandon it without losing legitimacy. They are the primary interlocutors for the state on personal law reform.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, communal_elites, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, communal_elites, beneficiary).

% The national parliament, which possesses formal legislative supremacy but is politically constrained from enacting a Uniform Civil Code (UCC) because the consociational bargain treats personal law fragmentation as a stability guarantee. Legislative paralysis on family law is not dysfunction but the intended operation of the mechanism — the legislature's inability to override community codes is the feature that prevents majoritarian domination. The legislature can act only with broad consensus across community elites.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, state_legislature, agenda_setter,
    institutional, biographical, constrained, national).

% The religious/ethnic majority whose preferred family law norms cannot be imposed nationally because the fragmented authority structure blocks majoritarian legislation. They bear the cost of legal pluralism — a patchwork of personal laws instead of a uniform code reflecting majority values. Their exit is mobile: they can advocate for UCC through democratic channels, but the consociational structure raises the threshold for success. They are not trapped; they are structurally blocked.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_community, payer,
    powerful, biographical, mobile, national).

% Women within minority communities who are subject to community personal laws that may discriminate in marriage, divorce, maintenance, and inheritance. They are not represented in the elite bargain that maintains fragmented authority. Their exit is identity-locked: leaving the community's legal framework often means leaving the community itself, with severe social and economic consequences. They would object to the consociational arrangement if they had voice — their exclusion is structural, not incidental.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_in_minority_communities, excluded,
    powerless, biographical, identity_locked, national).

% The apex court that adjudicates constitutional challenges to personal law codes. It operates as an analytical observer of the consociational structure but can become an agenda-setter through judicial review. Its exit is analytical — it observes the system from outside the bargain. The court's jurisprudence (judicial_harmonization_reading) creates structural pressure on the fragmented system by imposing constitutional equality floors without legislative action.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Secular organizations, women's rights groups, and liberal reformers who advocate for a Uniform Civil Code and view legal pluralism as an anachronism that entrenches gender inequality. They are excluded from the consociational bargain — the elite bargain does not include them as parties. Their exit is mobile: they can mobilize public opinion, litigate, and lobby for legislative change, but the fragmented authority structure is designed to resist exactly this pressure.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_civil_society, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian domination in a deeply plural society by fragmenting marriage authority across community-specific personal law codes. The coordination problem solved is: how do diverse communities with irreconcilable family law norms coexist in one polity without one community's norms becoming the national standard? The answer is a consociational elite bargain that makes legislative action on family law require cross-community consensus, turning paralysis into stability.
% TRANSFER_FUNCTION: Transfers legislative authority over family law from the national legislature to community elites. The state retains formal supremacy but cedes effective jurisdiction. The transfer moves decision-making power from a majoritarian arena (parliament) to a consociational arena (community elites + state negotiation). No direct resource transfer — the transfer is jurisdictional and symbolic.
% ABSENT_VOICES: Women within minority communities are the primary absent voice — they are subject to personal laws negotiated by male-dominated communal elites but have no seat at the bargaining table. Secular civil society and reformist voices within communities are also excluded. They are absent because the consociational model recognizes only corporate community representatives, not individual rights-holders or cross-cutting civil society actors.
% DISAPPEARANCE_RATIONALE: If the fragmented authority structure vanished overnight, the national legislature would face immediate pressure to enact a Uniform Civil Code. Majority community norms would likely become the default. Minority communities would lose their protected jurisdictional space. Communal elites would lose their administrative monopoly. Women in minority communities might gain access to more egalitarian statutory law but would lose community belonging. The polity would reorganize around majoritarian legislative politics rather than consociational negotiation.
% FOUNDING_PROBLEM: Post-colonial state formation in a religiously plural society where the majority community could use legislative supremacy to impose its family law norms on minorities, threatening communal cohesion and state stability. The founding problem was: how to constitutionalize pluralism so that no single community's family law becomes the law of the land?
% FOUNDING_PROBLEM_CORROBORATION: Constituent Assembly debates record the deliberate choice to place personal law in the Concurrent List and omit a UCC directive from enforceable provisions — attested by constitutional historians (Granville Austin, Upendra Baxi) outside the beneficiary communities. Communal elites attest the problem remains live (minorities still need protection). Secular reformers and gender rights advocates attest the founding problem is dead (majoritarian domination is no longer the primary threat; intra-community gender inequality is). The status is contested because the threat the mechanism was built against has shifted.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's primary operation is jurisdictional allocation, not resource extraction. The state cedes authority; it does not collect rents. Suppression is low-moderate (0.22) — the constraint suppresses majoritarian legislation but does not coerce individuals directly (coercion operates within communities, not by the constraint itself). Theater ratio is moderate (0.31) — the consociational rhetoric of 'protecting minorities' increasingly covers elite capture and gender inequality, but the coordination function (preventing majoritarian imposition) remains real. Accessibility collapse is moderate (0.42) — alternatives (UCC, judicial harmonization) exist and are actively pursued but face high structural barriers. Resistance is moderate (0.38) — from secular reformers, gender rights advocates, and judicial pressure, but the consociational structure absorbs rather than crushes resistance.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (minority_communities, communal_elites) experience this as a protective rope — coordination without extraction. The excluded seat (women_in_minority_communities) experiences it as a snare — their inequality is entrenched by the very mechanism that 'protects' their community. The payer seat (majority_community) experiences it as a tangled_rope — genuine coordination (pluralist stability) with asymmetric cost (their norms blocked). The engine computes this divergence from the structural data; the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority communities and communal elites are beneficiaries (d near 0.0) — the constraint subsidizes their jurisdictional autonomy. The majority community is a payer (d ~ 0.6) — it bears the cost of blocked legislative preference but has mobile exit (democratic advocacy). Women in minority communities are excluded with identity_locked exit (d ~ 0.85) — they bear intra-community inequality amplified by the constraint's protection of community autonomy, but cannot exit without identity rupture. The state legislature is agenda_setter with constrained exit (d ~ 0.4) — it administers the paralysis but cannot unilaterally change it. The Supreme Court is observer (d ~ 0.5) — analytical seat with growing agenda-setter pressure via judicial review.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built for inter-community protection (majoritarian domination threat). That threat has attenuated but not vanished. Meanwhile, an intra-community threat (gender inequality) has become salient. The mechanism does not address the new threat — it was not built for it. This is not mandatrophy (the original function persists) but functional mismatch. The classification prevents mislabeling: calling this a snare would miss the genuine coordination function; calling it a pure rope would miss the gendered extraction it enables. The tangled_rope classification would apply if we centered women's experience, but this reading centers the consociational bargain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the federalist_millet_reading a distinct structural constraint from the communal_autonomy_reading, or do they describe the same arrangement from different rhetorical angles?',
    'Compare beneficiary/victim structures: federalist_millet centers minority_communities as anti-majoritarian beneficiaries with state as constrained agenda_setter; communal_autonomy centers communal_elites as tradition-bearers with state as enforcer. If the victim sets differ (women_in_minority_communities appear only in gender_rights_reading), the readings are structurally distinct.',
    'If readings are structurally identical, they should be one constraint story. If distinct, the kernel decomposition is validated and each gets its own ε. The federalist_millet_reading''s ε=0.18 assumes the coordination function is genuine; if communal_autonomy_reading is the same constraint, ε may be higher due to elite capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the federalist_millet and communal_autonomy readings decompose into one or two constraints.').

omega_variable(
    legislative_paralysis_feature_or_bug,
    'Is the legislature''s inability to enact UCC a deliberate stability feature of the consociational bargain, or an unintended rigidity that now blocks necessary reform?',
    'Trace constituent assembly intent vs. contemporary operation. If the framers explicitly designed personal law fragmentation as a consensus-requiring mechanism (Austin, Baxi), it is a feature. If paralysis emerged from political avoidance not design, it is a bug. The engine''s T17 drift detection on suppression_requirement trajectory (rising from 0.10 to 0.22) signals increasing enforcement cost to maintain the paralysis.',
    'If feature, the constraint remains a rope (coordination via deliberate paralysis). If bug, rising suppression_requirement signals drift toward piton (theatrical maintenance of a failed coordination mechanism) or tangled_rope (paralysis now extracts by blocking gender reform).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_paralysis_feature_or_bug, empirical, 'Whether legislative paralysis on family law is a designed feature or an emergent pathology.').

omega_variable(
    elite_representation_ambiguity,
    'Do communal_elites genuinely represent minority_communities, or do they capture the consociational bargain to entrench patriarchal authority?',
    'Empirical study of community decision-making: do women and reformist minorities have voice in personal law administration? Comparative analysis of communities where elite bargaining succeeded vs. failed to protect vulnerable members. The gender_rights_reading''s victim structure (women_in_minority_communities) tests this empirically.',
    'If elites capture the bargain, the beneficiary declaration (minority_communities) is overbroad — the real beneficiary is communal_elites, and women_in_minority_communities are victims. This would shift the constraint toward tangled_rope (coordination for elites, extraction from women) and trigger FSM if claimed as mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_representation_ambiguity, empirical, 'Whether the consociational elite bargain represents communities or captures them.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.22) structural (the constraint blocks legislative majoritarianism) or internalized (women accept community authority because alternatives are unimaginable)?',
    'Post-exit trajectory analysis: women who leave community personal law systems (via secular marriage, conversion, migration) — does suppression persist? If internalized, the constraint''s effective suppression on women is higher than the structural measure. The identity_locked exit for women_in_minority_communities suggests internalization is significant.',
    'If suppression is partly internalized, the constraint''s extraction on women is underestimated by the structural measure. The federalist_millet_reading''s low ε (0.18) would not capture the gendered extraction that the gender_rights_reading centers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression on women within minority communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_fmr_tr_t1950, marriage_authority__federalist_millet_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(marriage_authority_fmr_tr_t1975, marriage_authority__federalist_millet_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(marriage_authority_fmr_tr_t1985, marriage_authority__federalist_millet_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(marriage_authority_fmr_tr_t2000, marriage_authority__federalist_millet_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(marriage_authority_fmr_tr_t2010, marriage_authority__federalist_millet_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement(marriage_authority_fmr_tr_t2025, marriage_authority__federalist_millet_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(marriage_authority_fmr_be_t1950, marriage_authority__federalist_millet_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(marriage_authority_fmr_be_t1975, marriage_authority__federalist_millet_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(marriage_authority_fmr_be_t1985, marriage_authority__federalist_millet_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(marriage_authority_fmr_be_t2000, marriage_authority__federalist_millet_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(marriage_authority_fmr_be_t2010, marriage_authority__federalist_millet_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(marriage_authority_fmr_be_t2025, marriage_authority__federalist_millet_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_fmr_su_t1950, marriage_authority__federalist_millet_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(marriage_authority_fmr_su_t1975, marriage_authority__federalist_millet_reading, suppression_requirement, 1975, 0.15).
narrative_ontology:measurement(marriage_authority_fmr_su_t1985, marriage_authority__federalist_millet_reading, suppression_requirement, 1985, 0.18).
narrative_ontology:measurement(marriage_authority_fmr_su_t2000, marriage_authority__federalist_millet_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(marriage_authority_fmr_su_t2010, marriage_authority__federalist_millet_reading, suppression_requirement, 2010, 0.21).
narrative_ontology:measurement(marriage_authority_fmr_su_t2025, marriage_authority__federalist_millet_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint (federalist_millet_reading) and communal_autonomy_reading share the same empirical referent (fragmented personal law system) but differ in ε: federalist_millet claims low-extraction rope (anti-majoritarian coordination); communal_autonomy claims higher extraction (elite capture of tradition). The secularist_reading and judicial_harmonization_reading are downstream pressures — they gain force as the consociational bargain erodes. The gender_rights_reading is the orthogonal challenger: it re-centers the victim structure on women_in_minority_communities, revealing extraction the federalist_millet_reading backgrounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, powerless, 0.85).
constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, organized, 0.15).
constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
