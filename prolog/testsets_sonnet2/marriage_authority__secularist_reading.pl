% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Legislative Marriage Authority Reading — Uniform Civil Code Mandate
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   The secularist reading of the marriage authority kernel holds that family
 *   law authority properly belongs to the democratic legislature acting for
 *   the citizenry as a whole, and that the current patchwork of
 *   community-administered personal laws is a transitional anomaly — a
 *   colonial-era holdover that a genuinely modern, equal citizenship must
 *   eventually eliminate through a Uniform Civil Code. This reading is one of
 *   five live readings of the same kernel (communal_autonomy,
 *   federalist_millet, gender_rights, judicial_harmonization, secularist).
 *   Each reading treats a structurally different arrangement as the object of
 *   contest, and this story authors ONLY the secularist reading's own account
 *   of the standing arrangement — the current pluralist personal-law regime
 *   it seeks to end, evaluated from the secularist coalition's own analytical
 *   lights. It does not average across readings, and it does not describe the
 *   sibling readings' preferred end states as if they were this constraint.
 *
 * KEY AGENTS:
 *   - secular_modernist_coalition: agenda_setter/beneficiary — drives UCC campaign, gains institutional standing
 *   - national_legal_uniformity_administrators: beneficiary — expanded jurisdictional mandate under a single code
 *   - gender_equality_litigation_bar: beneficiary — more tractable litigation target under uniformity
 *   - minority_religious_communities: payer — loses communal self-governance over family law
 *   - tribal_customary_law_holders: payer — structurally invisible, no leverage in the legislative process
 *   - personal_law_board_authorities: payer/agenda_setter — loses adjudicatory function and revenue
 *   - women_within_personal_law_regimes: excluded — invoked by both sides, seated by neither
 *   - constitutional_courts: observer — adjudicates but does not campaign for either uniformity or pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.71).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Legislative Marriage Authority Reading — Uniform Civil Code Mandate").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/political/religious").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'fe7abb43-be10-4d1c-b0cc-4160a0ad2323').
narrative_ontology:cs_kernel_codification('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', distributed).
narrative_ontology:cs_authority_grounding('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', distributed).
narrative_ontology:cs_reading_relation('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', foundational, legislature_is_sole_legitimate_author_of_family_law).
narrative_ontology:cs_axiom_status(legislature_is_sole_legitimate_author_of_family_law, holdable).
narrative_ontology:cs_axiom_grounding('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', legislature_is_sole_legitimate_author_of_family_law, conventional).
narrative_ontology:cs_axiom('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', foundational, personal_law_pluralism_is_transitional_not_constitutive).
narrative_ontology:cs_axiom_status(personal_law_pluralism_is_transitional_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', personal_law_pluralism_is_transitional_not_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', colonial_era_personal_law_settlement).
narrative_ontology:cs_drift_state('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', contemporary_ucc_debate_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fe7abb43-be10-4d1c-b0cc-4160a0ad2323', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, national_legal_uniformity_administrators).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_litigation_bar).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, tribal_customary_law_holders).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, personal_law_board_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislators, secular civil-society organizations, and constitutional-reform advocates who campaign for a Uniform Civil Code and treat personal law pluralism as a temporary constitutional embarrassment scheduled for correction. They set the legislative agenda, draft model codes, and gain political and institutional standing each time uniformity advances. Their exit from the debate is effectively arbitrage — they can reframe losses as incremental gains and are not personally subject to whichever family law regime prevails.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary).

% Ministries and law commissions tasked with drafting and administering a single national family code. A single code simplifies their caseload, court administration, and cross-state legal recognition, and expands their jurisdictional mandate at the expense of community-administered personal law bodies.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, national_legal_uniformity_administrators, beneficiary,
    institutional, generational, arbitrage, national).

% Lawyers and advocacy groups who litigate against discriminatory personal law provisions find a uniform code a more tractable target and a more durable win than piecemeal community reform. They gain professional standing and precedent-setting opportunities from the secularist push, though their interest in gender equality is only partially aligned with the coalition's uniformity-for-its-own-sake position.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_litigation_bar, beneficiary,
    organized, biographical, mobile, national).

% Communities whose marriage, divorce, and inheritance norms are currently administered under their own personal law. A Uniform Civil Code would replace these norms with a state-authored civil code regardless of community consent. They cannot exit the polity to preserve their family law and experience the secularist campaign as an existential threat to communal self-governance, not merely a technical legal reform.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, trapped, national).

% Indigenous and tribal groups whose customary marriage and inheritance practices are constitutionally protected exceptions to general civil law. A uniform code drafted around majority religious-community patterns threatens to override customary practices that were never organized around the majority/minority religious axis the secularist debate assumes. They have essentially no leverage in the national legislative process that would decide their fate.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, tribal_customary_law_holders, payer,
    powerless, generational, trapped, regional).

% Religious boards and clerical bodies that currently adjudicate marriage, divorce, and inheritance for their communities. A UCC would strip their adjudicatory function and revenue from marriage-related proceedings. They can lobby, litigate, and mobilize community resistance, but cannot exit a jurisdiction where the legislature holds ultimate constitutional authority over family law.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, personal_law_board_authorities, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, personal_law_board_authorities, agenda_setter).

% Women subject to discriminatory provisions within some personal law codes who might benefit from either a uniform code or intra-community reform, but who are not the deciding party in either the secularist legislative push or the personal law boards' resistance to it. Their interests are invoked by both sides but they hold no independent seat in the negotiation.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, women_within_personal_law_regimes, excluded,
    powerless, biographical, trapped, local).

% Adjudicate disputes between personal law provisions and constitutional equality guarantees, and rule on the constitutionality of any enacted Uniform Civil Code. Their rulings shape which reading of marriage authority becomes operative but they do not themselves campaign for uniformity or for pluralism.
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
% COORDINATION_FUNCTION: A single, legislatively-enacted civil code would let courts, registrars, and inter-jurisdictional actors apply one settled body of family law instead of adjudicating which of several community codes governs a given marriage, divorce, or inheritance dispute — genuinely reducing administrative and cross-community legal friction.
% TRANSFER_FUNCTION: Moves adjudicatory authority, revenue from marriage-related proceedings, and normative control over family life from religious personal law boards and communal institutions to the national legislature and its administrative apparatus, while moving legal certainty and (in the coalition's account) gender-equality protections toward individuals currently subject to discriminatory community provisions.
% ABSENT_VOICES: Women within personal law regimes are cited by the secularist coalition as the intended beneficiaries of uniformity, but are not themselves a negotiating party — the debate is conducted between the legislature/coalition and the personal law boards, with women's actual preferences (which vary and are not uniform) largely unascertained. Tribal customary law holders are structurally invisible to a debate framed around majority/minority religious communities.
% DISAPPEARANCE_RATIONALE: If the secularist legislative push for a Uniform Civil Code vanished, personal law boards would retain unchallenged adjudicatory authority over marriage and inheritance for their communities, litigation strategies built around anticipated uniformity would collapse, and the constitutional courts would remain the primary venue for incremental equality claims rather than facing pressure to defer to or preempt a prospective code.
% FOUNDING_PROBLEM: At independence, the state inherited a patchwork of colonial-era religious personal laws and needed to decide whether family law would be unified under the new constitution's citizenship or left administered by religious community authority; the secularist position holds that a genuinely equal, modern citizenship requires eventually resolving this in favor of legislative uniformity.
% FOUNDING_PROBLEM_CORROBORATION: The secular modernist coalition and constitutional law commissions attest the founding problem (unequal citizenship under fragmented personal law) remains live and unresolved. Minority religious community representatives and several constitutional scholars outside the coalition attest that the 'transitional anomaly' framing itself is what is contested — pluralism was constitutionally entrenched as a deliberate anti-majoritarian settlement, not an oversight awaiting correction, and no consensus exists on whether the founding problem was ever what the secularist reading claims it was.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored high (0.71 at interval end) because, from the secularist reading's own analytical lights on the standing pluralist arrangement, personal law boards extract adjudicatory authority and revenue from community members with no meaningful individual exit, and this extraction is what the UCC campaign exists to correct. Suppression (0.62) reflects that the standing arrangement is maintained by communal enforcement (social and religious sanction against exit from personal law) rather than by voluntary participation alone. Accessibility_collapse is moderate (0.5) — alternatives (civil marriage options, inter-community legal migration) exist in some jurisdictions but are constrained. Resistance is high (0.72): personal law boards and minority communities actively and organizedly resist any uniformity push, which is itself evidence the current arrangement functions as a defended structure, not settled consensus. Theater ratio is low-moderate and rises modestly (0.15→0.28) as legislative uniformity campaigns increasingly perform reform commitment without enacting a code — decades of law commission reports without legislative action.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular modernist coalition and allied administrative/litigation bodies are structural beneficiaries: they gain jurisdiction, standing, and precedent from every step toward uniformity, with arbitrage-grade exit from any political cost (they are not themselves subject to the personal law being reformed). Minority religious communities and personal law board authorities are structural targets: the campaign's success directly strips their governing authority and community coherence, and they have no meaningful individual or collective exit from a national legislature's jurisdiction. Tribal customary law holders are targets with essentially zero power in the process, positioned near the extreme target end despite being only tangentially related to the majority-religion-versus-secular-state framing that dominates the debate.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's own founding-problem answer treats the coordination function (a single, more efficient family-law regime) as still fully live and yet-unrealized — the code hasn't been enacted, so from the secularist seat mandatrophy language of 'obsolete function, persisting mandate' doesn't apply to their goal. But the classification correctly separates the coordination claim from the extraction data: even under the secularist's own account, the standing arrangement being contested (community-administered personal law) exhibits both a real coordination function for the communities it currently serves AND asymmetric extraction from members with no exit — hence tangled_rope, not snare, at this reading's own evaluation of the arrangement it seeks to replace.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_anomaly_vs_deliberate_design,
    'Was personal law pluralism a deliberate, considered constitutional settlement (an anti-majoritarian design choice, per the federalist_millet reading) or an unresolved colonial holdover awaiting legislative correction (per this secularist reading)?',
    'Constitutional drafting history and convention debates; comparative analysis of whether similarly-situated post-colonial states that retained personal law pluralism did so as considered policy or administrative default.',
    'If deliberate design, the secularist reading''s core premise — that pluralism is merely transitional and awaiting elimination — is substantially weakened, since removing it would violate the settlement''s original anti-majoritarian purpose rather than complete an interrupted unification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_anomaly_vs_deliberate_design, conceptual, 'Whether the kernel''s pluralism is an oversight or a foundational design choice — the central fault line between this reading and federalist_millet_reading.').

omega_variable(
    gender_equality_instrumentalization,
    'Does the secularist coalition''s invocation of gender equality genuinely track the interests of women within personal law regimes, or does it instrumentalize gender-equality rhetoric to advance a uniformity goal that is independently motivated by administrative and majoritarian-nationalist considerations?',
    'Survey and testimony from women within personal law regimes on their actual preferences between UCC, judicial reform, and communal reform; comparison of secularist coalition positions on gender equality in contexts unrelated to personal law.',
    'If instrumentalized, the beneficiary structure authored here (which lists gender_equality_litigation_bar as an aligned beneficiary) should be revisited to separate genuine equality advocates from uniformity-for-its-own-sake actors — this would not change this story''s ε but would refine which sibling reading (gender_rights_reading) more accurately represents the equality-seeking constituency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equality_instrumentalization, empirical, 'Whether gender-equality framing in the secularist coalition tracks genuine advocacy or instrumentalizes it for a distinct uniformity agenda.').

omega_variable(
    ucc_naturality_of_national_unity,
    'Is legislative supremacy over family law a natural incident of modern statehood and equal citizenship (as this reading assumes), or is that assumption itself a constructed, contestable claim that benefits identifiable actors (the secular modernist coalition, national administrators)?',
    'Comparative constitutional analysis of federal and consociational democracies that retain family-law pluralism without being judged less democratically legitimate or less equal in citizenship terms.',
    'If the naturality assumption is constructed rather than discovered, this reading''s framing of the current arrangement as merely ''transitional'' loses force, and the tangled_rope classification''s coordination-function claim (efficiency of a single code) should be weighted against evidence that pluralist regimes can deliver comparable administrative coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_naturality_of_national_unity, conceptual, 'Whether legislative uniformity in family law is a natural incident of modern statehood or a constructed preference serving identifiable beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__secularist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__secularist_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__secularist_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__secularist_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__secularist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marr_be_t8, marriage_authority__secularist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(marr_be_t16, marriage_authority__secularist_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(marr_be_t24, marriage_authority__secularist_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(marr_be_t32, marriage_authority__secularist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(marr_be_t40, marriage_authority__secularist_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t8, marriage_authority__secularist_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(marr_su_t16, marriage_authority__secularist_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(marr_su_t24, marriage_authority__secularist_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(marr_su_t32, marriage_authority__secularist_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(marr_su_t40, marriage_authority__secularist_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five readings of the marriage_authority kernel. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure and classification: communal_autonomy_reading (likely rope or tangled_rope from the community's own lights, with the secularist coalition as an external threat rather than an internal party), federalist_millet_reading (likely rope, treating pluralism itself as the coordination good preventing majoritarian tyranny), gender_rights_reading (likely tangled_rope or snare depending on which personal law provisions are at issue, with women as victims of intra-community rather than inter-community extraction), and judicial_harmonization_reading (likely scaffold, since case-by-case constitutional-floor review is explicitly transitional pending eventual codification or settlement). This secularist_reading treats the standing personal-law-pluralist arrangement as substantially extractive from the coalition's own analytical seat, and treats the eventual Uniform Civil Code as the (unrealized) remedy — not as the object being classified here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
