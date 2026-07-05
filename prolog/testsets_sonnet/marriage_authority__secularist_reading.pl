% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Uniform Civil Code Mandate — Secularist/Legislative-Supremacy Reading
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates the secularist reading of the marriage-authority
 *   kernel: the claim that legitimate authority over marriage, divorce,
 *   inheritance, and custody belongs exclusively to the democratically
 *   elected national legislature, and that the current pluralism of religious
 *   personal law systems is a transitional colonial-era anomaly to be
 *   eliminated through a Uniform Civil Code. Structurally this reading is a
 *   zero-sum challenge to the communal-autonomy reading: where that reading
 *   treats community religious tradition as the proper source of family-law
 *   norms with the state merely enforcing them, this reading treats the
 *   state's legislative authority as primary and communal law as an artifact
 *   awaiting supersession. The two readings cannot both be true of the same
 *   legal order at the same moment — they disagree about who holds original
 *   authority, not merely about policy preference. As authored here, this
 *   reading presents as genuine democratic coordination (equal citizenship,
 *   legal predictability, national integration) but its actual operation is
 *   substantially extractive: it transfers legal and cultural authority from
 *   organized, non-mobile minority communities to a coalition whose own
 *   practices already match the proposed uniform template, and it requires
 *   increasing legislative and judicial enforcement pressure to advance
 *   against organized communal resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.71).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Uniform Civil Code Mandate — Secularist/Legislative-Supremacy Reading").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'ec4ddd34-7c83-4f2d-8ced-9cec29168aa1').
narrative_ontology:cs_kernel_codification('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', distributed).
narrative_ontology:cs_authority_grounding('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', distributed).
narrative_ontology:cs_reading_relation('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', foundational, legislature_holds_original_authority_over_family_law).
narrative_ontology:cs_axiom_status(legislature_holds_original_authority_over_family_law, holdable).
narrative_ontology:cs_axiom_grounding('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', legislature_holds_original_authority_over_family_law, conventional).
narrative_ontology:cs_axiom('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', foundational, personal_law_pluralism_is_transitional_not_constitutive).
narrative_ontology:cs_axiom_status(personal_law_pluralism_is_transitional_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', personal_law_pluralism_is_transitional_not_constitutive, instrumental).
narrative_ontology:cs_reference_frame('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', post_independence_directive_principle_uniformity_mandate).
narrative_ontology:cs_drift_state('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', contemporary_communal_mobilization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ec4ddd34-7c83-4f2d-8ced-9cec29168aa1', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, national_legislature).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, unitary_judiciary_apparatus).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, personal_law_boards).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, customary_tribal_law_practitioners).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, civic_nationalism_over_communal_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political and civil-society bloc advocating a Uniform Civil Code as the completion of constitutional secularism. Gains legitimacy, electoral capital, and doctrinal victory each time personal law pluralism is narrowed by legislation or reclassified as anomaly. Faces no personal cost from UCC adoption since its own family arrangements already track the civil-code template it wants universalized.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter).

% Holds constitutional authority to enact a Uniform Civil Code under this reading's premise that marriage regulation is properly legislative, not communal. Drafts, debates, and can pass UCC bills; enforcement of any resulting code runs through courts and civil registries it controls.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, national_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from a single, codified family-law regime that reduces the interpretive burden of adjudicating among multiple religious codes. A uniform code simplifies case law, standardizes precedent, and removes recurring jurisdictional disputes between personal law boards and civil courts.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, unitary_judiciary_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Practice marriage, divorce, inheritance, and custody under community-specific personal law. Under this reading their entire family-law tradition is recast as a transitional anomaly to be legislated out of existence. Exit means either abandoning communal legal identity or resisting through political mobilization; there is no arbitrage — leaving the jurisdiction is the only true exit and most cannot.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, constrained, national).

% Community institutions that currently administer marriage, divorce, and inheritance rules for their members. A UCC would strip their adjudicatory function entirely, transferring authority to state civil registries and courts. They can lobby, litigate, or mobilize street protest, but cannot exit the jurisdiction that could legislate them out of function.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, personal_law_boards, payer,
    organized, generational, constrained, national).

% Indigenous and tribal communities whose customary marriage and inheritance practices fall outside both mainstream religious personal law and any drafted UCC template. Frequently invisible in the secularist framing, which treats 'personal law pluralism' as a communal-religious problem and rarely accounts for non-religious customary systems at all. Have essentially no institutional voice in the legislative process.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, customary_tribal_law_practitioners, payer,
    powerless, biographical, trapped, regional).

% Women within minority communities who seek reform of discriminatory personal law provisions but do not want their communities' entire legal identity dissolved into a state code drafted primarily by the majority-community-dominated legislature. Their preferred path — targeted equality litigation within personal law, not wholesale replacement — is largely absent from the secularist framing, which treats gender equality and communal elimination as the same project.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, intra_community_women_reformers, excluded,
    moderate, biographical, constrained, national).

% Adjudicate constitutional challenges to both personal law provisions and any UCC legislation. Can validate, strike down, or require modification of a uniform code; their doctrine on Article-level directive principles versus fundamental rights determines how much force this reading's legislative mandate actually carries.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable civil-law framework for marriage, divorce, inheritance, and custody applicable to all citizens regardless of religion — reducing forum-shopping, cross-community legal conflicts, and jurisdictional ambiguity in a religiously plural state.
% TRANSFER_FUNCTION: Moves adjudicatory authority, legal legitimacy, and administrative control over family law from community religious institutions and personal law boards to the national legislature and unitary judiciary; moves cultural and legal self-determination away from minority religious and customary communities toward the secular-modernist coalition's preferred civic template.
% ABSENT_VOICES: Customary tribal law practitioners are almost entirely absent from the secularist framing, which conceives the problem as religious-personal-law-versus-state rather than accounting for non-religious customary systems. Intra-community women reformers who want targeted equality reform without wholesale communal legal dissolution are also structurally excluded — the reading treats gender-equality concerns and communal elimination as a single bundled objective.
% DISAPPEARANCE_RATIONALE: If the secularist mandate for a Uniform Civil Code disappeared as a legislative goal overnight, personal law systems would continue operating largely as before — from the minority communities' vantage, the world would barely change. But from the secular-modernist coalition and constitutional-supremacy advocates' vantage, a core unfinished nation-building project would be permanently abandoned, and they contest that this counts as 'no change.' The verdict genuinely depends on which seat is asked.
% FOUNDING_PROBLEM: Colonial-era administrations codified separate personal law systems for different religious communities to govern their own family matters; post-independence constitution-makers left this pluralism in place while directive principles gestured toward eventual uniformity, creating an unresolved question of whether marriage law should ultimately be communal or national.
% FOUNDING_PROBLEM_CORROBORATION: The secular-modernist coalition and much of the national legislature attest the founding problem (communal fragmentation impeding equal citizenship) is still live and demands legislative resolution. Independent legal historians and comparative constitutional scholars outside both the secularist coalition and the personal law boards note that the 'transitional anomaly' framing was itself a specific post-independence political choice, not a constitutional inevitability — corroboration for the 'still live and legislatively mandated' status is largely internal to the coalition that benefits from resolving it that way.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, contested).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.71 over the interval as the secularist coalition's mandate hardens from constitutional aspiration (Article-level directive principle, largely dormant) into active legislative and judicial pressure — courts increasingly cited as vehicles for de facto uniformity even absent formal UCC passage, and legislative drafting committees actively soliciting model codes. Suppression_requirement tracks this same hardening (0.38 to 0.62): as personal law boards and minority communities organize resistance, the coalition's mechanisms for advancing the code (public campaigns delegitimizing personal law as 'backward,' judicial nudging, administrative registry defaults) require increasingly active enforcement machinery. Theater_ratio rises modestly (0.18 to 0.34) reflecting growing performative invocation of 'gender justice' and 'national integration' rhetoric that outpaces the actual drafting progress of any concrete UCC text — much of the visible activity is symbolic positioning rather than legislative substance, though a genuine coordination core (reducing multi-forum family law conflict) persists underneath it.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular-modernist coalition and unitary judiciary sit near the full-beneficiary end: they collect legitimacy, doctrinal victory, and administrative simplification, and their own family arrangements already conform to the civil-code template they are universalizing, so a UCC costs them nothing personally. Minority religious communities and personal law boards sit near the full-target end: they lose an entire domain of communal self-governance, have organized power but constrained exit (their only true exit is leaving the jurisdiction, which is not viable for the vast majority), and bear the transfer directly. Customary tribal law practitioners are positioned as trapped and powerless — they are not even the primary object of the secularist-versus-communal contest, yet stand to lose customary legal recognition entirely if a UCC is drafted around the religious-personal-law binary and ignores non-religious customary systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial-era codification producing a fragmented family-law landscape — is genuinely contested rather than resolved: the secularist coalition treats it as still fully live and urgently unfinished, while comparative legal scholarship outside the coalition suggests 'transitional anomaly' was itself a specific ideological framing adopted after independence, not a constitutional necessity. This divergence is exactly what the R5 corroboration question is designed to surface: an origin story attested almost entirely by the parties who benefit from resolving it in their preferred direction is a weak genealogy, and the tangled_rope classification here should not be read as a verdict that the coordination function (legal predictability, reduced forum-shopping) is fake — only that its persistence and current acceleration are substantially explained by asymmetric extraction from organized minority communities, not by settled consensus that the anomaly needs eliminating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secularist_vs_communal_kernel_framing,
    'Is marriage authority properly located in the democratic legislature (this reading) or in community religious tradition with the state as enforcer only (the communal_autonomy_reading)? The two readings assign original authority to structurally incompatible sources.',
    'No empirical resolution exists — this is a foundational disagreement about the location of legitimate authority in a plural constitutional order, adjudicated (if at all) by constitutional convention, judicial doctrine on directive principles versus fundamental rights, or eventual political settlement.',
    'If the communal reading prevails as the operative constitutional doctrine, the entire premise of this constraint — that pluralism is a transitional anomaly awaiting elimination — dissolves, and the extraction this story documents would be recharacterized as majoritarian encroachment on protected communal autonomy rather than legitimate democratic legislative reach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularist_vs_communal_kernel_framing, conceptual, 'Foundational kernel-level disagreement between secularist and communal-autonomy readings of marriage authority.').

omega_variable(
    ucc_gender_equality_bundling,
    'Does the secularist reading''s pursuit of a Uniform Civil Code actually serve intra-community gender equality, or does it use gender-equality rhetoric to bundle a broader project of communal legal elimination that many women reformers do not endorse?',
    'Comparative study of jurisdictions that pursued targeted equality litigation within personal law (the gender_rights_reading path) versus full UCC replacement, tracking actual outcomes for women in each community versus stated legislative intent.',
    'If the gender-equality justification is substantially separable from the elimination project, then a large share of this constraint''s claimed coordination function (protecting vulnerable community members) is decorative, and the true extraction ratio is higher than the coordination-adjusted classification suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_gender_equality_bundling, empirical, 'Whether gender-equality justification for UCC is separable from wholesale communal legal displacement.').

omega_variable(
    customary_law_visibility_gap,
    'Does any drafted Uniform Civil Code account for non-religious customary and tribal law systems, or does the religious-personal-law-versus-state binary structurally erase them from the legislative conversation entirely?',
    'Textual analysis of actual UCC draft bills and legislative committee proceedings for explicit treatment (or omission) of customary/tribal marriage and inheritance systems.',
    'If customary systems are omitted, the true victim class of this constraint is broader than the religious-personal-law communities the secularist/communal debate foregrounds, and the powerless, trapped customary_tribal_law_practitioners bear extraction with essentially no representation in either side of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_visibility_gap, empirical, 'Whether customary/tribal law systems are addressed or erased in UCC drafting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__secularist_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__secularist_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__secularist_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__secularist_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__secularist_reading, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marr_be_t8, marriage_authority__secularist_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(marr_be_t16, marriage_authority__secularist_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(marr_be_t24, marriage_authority__secularist_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(marr_be_t32, marriage_authority__secularist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(marr_be_t40, marriage_authority__secularist_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(marr_su_t8, marriage_authority__secularist_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(marr_su_t16, marriage_authority__secularist_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(marr_su_t24, marriage_authority__secularist_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(marr_su_t32, marriage_authority__secularist_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(marr_su_t40, marriage_authority__secularist_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority kernel, each instantiated as its own constraint story with its own ε, stakeholders, and classification per the ε-invariance principle. The secularist reading claims legislative supremacy and treats pluralism as eliminable anomaly (ε_high tangled_rope, this story). The communal_autonomy_reading treats community tradition as the authoritative source with the state as mere enforcer (structurally near-foreclosed by this reading). The federalist_millet_reading treats fragmentation itself as an anti-majoritarian safeguard. The gender_rights_reading pursues equality via judicial expansion within personal law rather than wholesale replacement. The judicial_harmonization_reading achieves convergence via case-by-case constitutional review without formal UCC legislation, functioning as a slower-moving structural pressure on this reading's legislative mandate. All five are linked here as a constraint family; contamination or purity shifts in any sibling should be checked against this network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
