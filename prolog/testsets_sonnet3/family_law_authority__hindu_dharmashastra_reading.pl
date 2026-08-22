% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   This story authors the dharmashastric reading of the family_law_authority
 *   kernel: marriage understood as an indissoluble sacramental samskara (one
 *   of the classical life-cycle rites) whose validity and meaning are fixed
 *   by dharmic text (chiefly the Dharmashastra corpus, e.g. Manusmriti and
 *   its commentaries) and by regional customary practice, adjudicated by
 *   Brahminical priestly authority and enforced by joint-family patriarchal
 *   control over property. Colonial administration substantially calcified
 *   this reading into positive law by relying on Sanskrit-literate pandit
 *   informants to construct 'Hindu law' for British courts, freezing what had
 *   been a plural and regionally variable customary field into a more rigid
 *   textual orthodoxy. The postcolonial Indian state's 1955-56 Hindu Code
 *   Bills and the 2005 Hindu Succession Act amendment progressively displaced
 *   the sacramental-indissolubility and coparcenary-exclusion elements of
 *   this reading with statutory divorce, widow remarriage, and daughters'
 *   coparcenary rights — while leaving the samskara's ritual and social
 *   meaning intact for those who still observe it. This story's epsilon is
 *   authored for the classical/colonial-era arrangement AS THIS READING
 *   UNDERSTOOD IT, not for the post-1955 reformed regime, and not for the
 *   secular-contractual reading's endorsed alternative (per the
 *   epsilon-invariance rule for kernel readings).
 *
 * KEY AGENTS:
 *   - joint_family_patriarchs: agenda-setting beneficiary, administers property and marriage alliance
 *   - brahminical_priestly_authority: agenda-setting beneficiary, interpretive monopoly over ritual validity
 *   - hindu_wives_pre_1955: primary payer, ritual participant without exit
 *   - widows_barred_from_remarriage: payer, structurally excluded by indissolubility doctrine
 *   - inter_caste_couples: payer, excluded by endogamy norm
 *   - daughters_excluded_from_coparcenary: payer, structurally disinherited
 *   - colonial_and_postcolonial_state: excluded observer/eventual reformer, outside interpretive community
 *   - reform_movements_and_lower_caste_dissent: excluded contesting voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.71).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '82783031-e302-488b-97ac-6f2cb4cc69d2').
narrative_ontology:cs_kernel_codification('82783031-e302-488b-97ac-6f2cb4cc69d2', fixed_text).
narrative_ontology:cs_authority_grounding('82783031-e302-488b-97ac-6f2cb4cc69d2', lineage).
narrative_ontology:cs_interpretation_layer_present('82783031-e302-488b-97ac-6f2cb4cc69d2').
narrative_ontology:cs_reading_relation('82783031-e302-488b-97ac-6f2cb4cc69d2', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('82783031-e302-488b-97ac-6f2cb4cc69d2', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('82783031-e302-488b-97ac-6f2cb4cc69d2', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('82783031-e302-488b-97ac-6f2cb4cc69d2', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('82783031-e302-488b-97ac-6f2cb4cc69d2', foundational, marriage_as_indissoluble_sacramental_rite).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_sacramental_rite, overridden).
narrative_ontology:cs_axiom_grounding('82783031-e302-488b-97ac-6f2cb4cc69d2', marriage_as_indissoluble_sacramental_rite, theological).
narrative_ontology:cs_axiom('82783031-e302-488b-97ac-6f2cb4cc69d2', foundational, caste_endogamy_as_dharmic_requirement).
narrative_ontology:cs_axiom_status(caste_endogamy_as_dharmic_requirement, overridden).
narrative_ontology:cs_axiom_grounding('82783031-e302-488b-97ac-6f2cb4cc69d2', caste_endogamy_as_dharmic_requirement, conventional).
narrative_ontology:cs_axiom('82783031-e302-488b-97ac-6f2cb4cc69d2', secondary, coparcenary_property_transmission_through_male_line).
narrative_ontology:cs_axiom_status(coparcenary_property_transmission_through_male_line, overridden).
narrative_ontology:cs_axiom_grounding('82783031-e302-488b-97ac-6f2cb4cc69d2', coparcenary_property_transmission_through_male_line, conventional).
narrative_ontology:cs_reference_frame('82783031-e302-488b-97ac-6f2cb4cc69d2', classical_dharmashastric_samskara_order).
narrative_ontology:cs_drift_state('82783031-e302-488b-97ac-6f2cb4cc69d2', post_1955_codification_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('82783031-e302-488b-97ac-6f2cb4cc69d2', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_patriarchs).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, natal_family_lineage_status).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahminical_priestly_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, upper_caste_endogamy_order).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, hindu_wives_pre_1955).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows_barred_from_remarriage).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, daughters_excluded_from_coparcenary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, sons_and_natal_lineage_status).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, sanatana_dharma_continuity).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, samskara_ritual_efficacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the joint family estate (coparcenary), arranges marriages to consolidate land and caste standing, and invokes dharmic textual authority (Manusmriti, later commentaries) and customary practice to settle disputes. Retains control over property and ritual legitimacy regardless of how the marriage itself functions for the parties inside it.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_patriarchs, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, joint_family_patriarchs, beneficiary).

% Officiates the seven-steps (saptapadi) rite that dharmashastra treats as constitutive of the marriage, interprets ambiguous textual passages, and certifies ritual validity. Its interpretive monopoly is the mechanism through which caste-endogamy and indissolubility norms are transmitted and enforced across generations.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_priestly_authority, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, brahminical_priestly_authority, beneficiary).

% Enters marriage as a ritual participant whose consent is subordinate to family arrangement; the samskara is treated as indissoluble and irrevocable regardless of the marriage's condition. No independent right to divorce, limited or no independent property right, and social death (not merely economic hardship) attaches to attempts at exit.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_wives_pre_1955, payer,
    powerless, biographical, trapped, regional).

% Having undergone the samskara once, is dharmashastrically barred from a second sacramental union; loses coparcenary standing and is frequently reduced to a dependent or ascetic status within the natal or marital household. The sacramental-indissolubility premise that benefits family property consolidation directly produces this exclusion.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows_barred_from_remarriage, payer,
    powerless, biographical, trapped, regional).

% A union across caste lines is treated by dharmashastric interpretation as ritually invalid or degraded (anuloma/pratiloma hierarchy), stripping the couple of social recognition and often inheritance standing. The endogamy norm that stabilizes caste-based property and status transmission for incumbents falls on them as exclusion from legitimate marriage itself.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, payer,
    powerless, biographical, trapped, regional).

% Under classical dharmashastric joint-family property rules, daughters are excluded from coparcenary birthright (unlike sons), receiving only a marriage settlement or maintenance claim. Marriage functions as the mechanism transferring her out of the natal property line entirely.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, daughters_excluded_from_coparcenary, payer,
    powerless, generational, trapped, regional).

% Inherits coparcenary birthright by virtue of the same family-property regime that marriage sacramentality upholds; benefits from marriage alliances that consolidate rather than fragment family landholding and caste rank, though still bound by arranged-marriage obligation himself.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, sons_and_natal_lineage_status, beneficiary,
    moderate, generational, constrained, regional).

% British colonial courts largely deferred to Brahminical textual authorities (often via Orientalist pandits) to codify 'Hindu law,' freezing selective textual readings into administrable rule; the postcolonial Indian state later intervened directly (Hindu Marriage Act 1955, Hindu Succession Act 1956, its 2005 amendment) to override sacramental indissolubility and coparcenary exclusion. Neither state voice was inside the dharmashastric interpretive community it eventually overruled.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, colonial_and_postcolonial_state, excluded,
    institutional, civilizational, analytical, national).

% Nineteenth- and twentieth-century reform movements (Arya Samaj, Brahmo Samaj, Ambedkar-aligned Dalit critique) contested sacramental indissolubility, widow remarriage prohibition, and caste endogamy from within and against Hindu tradition, but were structurally outside the priestly-patriarchal interpretive authority that dharmashastra vested with adjudicative power.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reform_movements_and_lower_caste_dissent, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, joint_family_patriarchs).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, textually-anchored framework for transmitting property, caste status, and lineage continuity across generations through a ritually-marked, socially legible union, reducing disputes over inheritance and social standing by fixing marriage's meaning and permanence in advance.
% TRANSFER_FUNCTION: Moves control over reproductive alliance-making, property consolidation, and ritual legitimacy from individual spouses (especially wives) to family patriarchs and priestly interpreters; moves property specifically away from daughters and widows toward male coparceners.
% ABSENT_VOICES: Wives entering marriage as minors or under arranged terms had no interpretive standing in dharmashastric adjudication; widows and inter-caste couples affected by the endogamy and indissolubility rules were symbolically present in the texts as objects of regulation but absent from the priestly and patriarchal bodies that interpreted those texts.
% DISAPPEARANCE_RATIONALE: The 1955-56 codification (Hindu Marriage Act, Hindu Succession Act) demonstrates the counterfactual directly: when sacramental indissolubility and coparcenary exclusion were statutorily overridden, divorce became available, widow remarriage was legalized, and daughters eventually gained coparcenary parity (2005) — property and family arrangements measurably reorganized rather than continuing unchanged, confirming the arrangement was load-bearing rather than incidental.
% FOUNDING_PROBLEM: Provide a durable, sacred, socially unquestionable basis for lineage continuity, property transmission, and caste-status reproduction in a context without centralized secular family-law adjudication.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical commentators and joint-family patriarchs attest the samskara framework remains spiritually and socially necessary. Independent corroboration from outside the benefiting interpretive community is available: colonial-era ethnographic and legal-historical scholarship (e.g., on the codification process itself), the Indian Constituent Assembly debates preceding the Hindu Code Bills, and Ambedkar's own documented critique of caste-endogamous joint-family law all attest the founding problem (property/status transmission) persists but no longer requires sacramental indissolubility, caste endogamy, or coparcenary exclusion to be solved — those elements were legislatively severable and were in fact severed.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 in the base profile, reflecting roughly the pre-1955 steady state) is high because the sacramental-indissolubility and coparcenary-exclusion rules move real, durable value (property, remarriage rights, social standing) from wives, widows, and daughters to patriarchs and the priestly authority that certifies ritual legitimacy — this is not a symmetric coordination cost. Suppression (0.71) is high because the framework's persistence depended on active enforcement: social ostracism for widow remarriage, nullification or non-recognition of inter-caste unions, and (later) colonial court backing for a fixed textual reading over more flexible customary practice. Theater ratio (0.4, rising sharply after 1955 in the measurement series) tracks the growing gap between the samskara's still-performed ritual form and its now-attenuated legal-extractive function once statute overrode indissolubility and coparcenary exclusion — by 2005 the rite persists largely as cultural/religious performance rather than as the operative legal mechanism it once was. Accessibility collapse (0.6) and resistance (0.62) reflect that alternatives to arranged, endogamous, sacramental marriage were not fully closed off (customary variation, some regional flexibility, and reform movements existed) but were substantially foreclosed for most parties, especially women, without significant family or state backing.
 *
 * DIRECTIONALITY LOGIC:
 *   Joint family patriarchs and priestly authority sit at the low end of directionality: they set the interpretive and administrative terms and capture the coordination benefit (stable lineage transmission, consolidated property, caste-status reproduction) with mobile/arbitrage-grade exit from the constraint's costs. Wives, widows, inter-caste couples, and daughters sit at the high end: trapped exit options, powerless structural position, and the specific mechanism (sacramental indissolubility, endogamy, coparcenary exclusion) extracts directly from their marital, remarriage, and inheritance prospects. Sons occupy a genuinely intermediate position — beneficiaries of the coparcenary regime but still bound by arranged-marriage obligation, hence 'constrained' rather than 'mobile' exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (durable lineage/property/status transmission absent centralized secular adjudication) is authored as contested rather than flatly dead: the underlying coordination need for SOME family-law framework did not disappear, but the specific dharmashastric mechanisms achieving it (indissolubility, endogamy, coparcenary exclusion) were legislatively demonstrated to be severable from that coordination function — the state solved the same coordination problem without those extractive elements. This is why tangled_rope rather than pure snare: a genuine coordination function existed (that is what the 1955-56 codifiers preserved and reformed rather than abolished outright), but it was bundled with asymmetric extraction from wives, widows, and lower-caste/inter-caste parties that required active enforcement to sustain and that could be, and was, unbundled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authenticity_vs_colonial_construction,
    'How much of the ''classical dharmashastric'' family law as administered from 1800-1955 reflects continuous pre-colonial practice versus a colonial-era construction (via selective pandit consultation and case-law codification) that froze one textual strand into positive law?',
    'Comparative legal-historical scholarship on pre-colonial regional customary variation (e.g. matrilineal practices in parts of the south, more flexible divorce customs in some communities) versus the colonial Anglo-Hindu case law record.',
    'If substantially a colonial construction, the ''sacramental indissolubility'' this story authors as the classical dharmashastric position was itself already a narrowed, extraction-favoring selection from a more plural customary field — meaning even the pre-1955 baseline this story treats as the reading''s steady state was itself a product of an earlier extractive narrowing, not a stable natural baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authenticity_vs_colonial_construction, empirical, 'Whether the pre-1955 dharmashastric family law regime reflects continuous tradition or colonial-era textual selection.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the sacramental-indissolubility premise of this reading logically foreclose the secular-contractual reading''s premise (marriage as revocable contract between autonomous individuals) only for adherents who accept dharmashastric authority, or does it make a universal claim that would foreclose the secular reading for all parties regardless of religious identification?',
    'Examine whether classical dharmashastric sources assert jurisdiction over non-Hindu marriages or only over Hindu ritual practice; examine whether personal-law systems in India apply exclusively to self-identified adherents.',
    'If jurisdiction is understood as universal, reading_relations to secular_contractual_reading should be ''forecloses'' rather than ''coexists_with''; if jurisdiction is understood as community-bounded (as India''s personal law system in fact treats it), ''coexists_with'' is correct because both readings operate as live law for different populations simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether this reading''s indissolubility claim is jurisdiction-universal or community-bounded relative to the secular reading.').

omega_variable(
    reform_as_internal_or_external_correction,
    'Was the 1955-56 codification (which overrode sacramental indissolubility and coparcenary exclusion) an internal reform continuous with dharmashastric tradition''s own capacity for change (as some framers argued, citing textual precedent for regional/temporal variation in dharma) or an externally imposed secular-constitutional override of a tradition that could not have reformed itself?',
    'Examine the Constituent Assembly and Hindu Code Bill debates directly: proponents'' textual arguments for continuity versus opponents'' arguments that the Bills abandoned dharmashastric authority entirely in favor of constitutional equality principles.',
    'If internal reform, this reading''s own tradition should be understood as having partially self-overridden the sacramental-indissolubility axiom (supporting ''overridden'' status for that axiom within the tradition); if external imposition, the axiom remains ''holdable'' within the reading''s own frame even though state law no longer enforces it, and the drift_state ''acknowledged'' field should be authored as contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_as_internal_or_external_correction, conceptual, 'Whether the 1955-56 reform is genealogically continuous with or external to the dharmashastric tradition itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1800, 2005).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1800, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(fami_tr_t1850, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1850, 0.18).
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.24).
narrative_ontology:measurement(fami_tr_t1929, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1929, 0.3).
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.45).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(fami_tr_t2005, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 2005, 0.62).

% Extraction over time
narrative_ontology:measurement(fami_be_t1800, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1800, 0.74).
narrative_ontology:measurement(fami_be_t1850, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1850, 0.75).
narrative_ontology:measurement(fami_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.73).
narrative_ontology:measurement(fami_be_t1929, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1929, 0.71).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.58).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(fami_be_t2005, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 2005, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1800, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1800, 0.8).
narrative_ontology:measurement(fami_su_t1850, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1850, 0.82).
narrative_ontology:measurement(fami_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(fami_su_t1929, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1929, 0.72).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(fami_su_t1980, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(fami_su_t2005, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 2005, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings decomposing the colloquial concept 'marriage/family law authority in India' (the family_law_authority kernel). Each reading is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle: the personal-law system administers genuinely different legal regimes to different religious communities, so 'Indian family law' is not one constraint measured five ways but five constraints coexisting under one constitutional umbrella (Article 25-26 freedom of religion, tempered by Article 44's unrealized uniform civil code aspiration). The secular_contractual_reading is marked 'influences' rather than 'coexists_with' because the 1955-56 codification of this reading imported contract-like elements (registration, statutory divorce, consent formalization) that created downstream structural pressure toward convergence with the secular reading without fully collapsing this reading's residual sacramental and customary elements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
