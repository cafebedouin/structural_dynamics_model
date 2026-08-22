% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate for Palestine — Dual Obligation Reading (Existing Arab Rights Protection)
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This story instantiates the dual-obligation reading of the Palestine
 *   Mandate kernel: the position, held by Palestinian Arab political
 *   leadership and periodically validated by British commissions of inquiry,
 *   that the Mandate's text (Article 22 of the League Covenant, the Mandate
 *   instrument's own proviso clause) imposes an obligation to protect
 *   existing Arab civil, political, and property rights that is equal or
 *   superior to the Balfour 'national home' commitment. Under this reading,
 *   land-transfer restrictions, immigration ceilings pegged to absorptive
 *   capacity, and claims to eventual representative government proportional
 *   to the Arab majority are not concessions but the legally correct reading
 *   of the instruments. The sibling readings — jewish_national_home_primacy
 *   (which reads the same text as directing demographic transformation toward
 *   Jewish sovereignty) and mandatory_interpretive_discretion (which locates
 *   the operative constraint in unreviewable British discretion rather than
 *   in either substantive reading) — are separate constraints, not alternate
 *   measurements of this one. Each reading has its own beneficiary/victim
 *   structure and its own epsilon; they are linked here only through the
 *   shared kernel and the network edges below.
 *
 * KEY AGENTS:
 *   - palestinian_arab_landholders: primary beneficiary of land-tenure protection language
 *   - palestinian_arab_political_elites: beneficiary of self-determination framing, but excluded from actual proportional government
 *   - palestinian_arab_peasant_cultivators: powerless beneficiary, trapped, dependent on enforcement they cannot compel
 *   - zionist_organizations: primary target — blocked from land acquisition and demographic parity by this reading's restrictions
 *   - jewish_immigrant_communities: secondary target — quota-capped entry
 *   - british_mandatory_administrators: dual-positioned payer/agenda_setter, squeezed by the contradiction between readings
 *   - permanent_mandates_commission: analytical observer with publicity-only enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.78).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.71).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate for Palestine — Dual Obligation Reading (Existing Arab Rights Protection)").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, '94fbc463-7303-4753-99c9-bae51449d60b').
narrative_ontology:cs_kernel_codification('94fbc463-7303-4753-99c9-bae51449d60b', fixed_text).
narrative_ontology:cs_authority_grounding('94fbc463-7303-4753-99c9-bae51449d60b', extraction).
narrative_ontology:cs_interpretation_layer_present('94fbc463-7303-4753-99c9-bae51449d60b').
narrative_ontology:cs_reading_relation('94fbc463-7303-4753-99c9-bae51449d60b', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('94fbc463-7303-4753-99c9-bae51449d60b', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('94fbc463-7303-4753-99c9-bae51449d60b', foundational, existing_population_rights_textually_superior).
narrative_ontology:cs_axiom_status(existing_population_rights_textually_superior, holdable).
narrative_ontology:cs_axiom_grounding('94fbc463-7303-4753-99c9-bae51449d60b', existing_population_rights_textually_superior, conventional).
narrative_ontology:cs_axiom('94fbc463-7303-4753-99c9-bae51449d60b', secondary, self_determination_grounds_majoritarian_sovereignty_claim).
narrative_ontology:cs_axiom_status(self_determination_grounds_majoritarian_sovereignty_claim, holdable).
narrative_ontology:cs_axiom_grounding('94fbc463-7303-4753-99c9-bae51449d60b', self_determination_grounds_majoritarian_sovereignty_claim, deontological).
narrative_ontology:cs_reference_frame('94fbc463-7303-4753-99c9-bae51449d60b', covenant_article_22_tutelage_primacy).
narrative_ontology:cs_drift_state('94fbc463-7303-4753-99c9-bae51449d60b', post_1930_commissions_of_inquiry, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('94fbc463-7303-4753-99c9-bae51449d60b', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landholders).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_political_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_peasant_cultivators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_immigrant_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, self_determination_norm_supremacy).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold customary and registered tenure over agricultural land that Zionist land-purchase agencies seek to acquire. Under this reading, the mandate's Article 6 caveat ('rights and position of other sections of the population not prejudiced') and general international-law protections for inhabitants of mandated territory are read as binding limits on land transfer and displacement. They benefit from restrictive land-transfer ordinances and from the mandatory power's periodic tightening of sale regulations, though enforcement is uneven and depends on which administration is in office.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landholders, beneficiary,
    moderate, generational, constrained, regional).

% Notable families, municipal councils, and the Arab Executive/Higher Committee structures invoke self-determination norms and the Covenant's Article 22 language about provisional recognition of independence to press for representative government proportional to the Arab majority. They benefit rhetorically and legally from this reading, but remain excluded from the legislative council schemes actually offered, since any proportional body would out-vote the national-home commitment the British will not abandon.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_political_elites, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_political_elites, excluded).

% Tenant farmers and fellahin displaced or threatened with displacement when absentee landlords sell to Jewish settlement agencies. This reading's tenancy-protection ordinances are meant to shield them from eviction, but they have no capacity to litigate their own protections and depend entirely on mandatory officials choosing to enforce the dual-obligation reading rather than the national-home reading in any given case.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_peasant_cultivators, beneficiary,
    powerless, biographical, trapped, local).

% The Jewish Agency and affiliated land-purchase and immigration bodies experience this reading as a direct obstacle: land transfer restrictions block acquisition needed for settlement, and immigration quotas capped to preserve Arab demographic majority prevent the population transfer necessary to make national-home language operative. They hold the Balfour Declaration and Mandate preamble as textual leverage but cannot force enforcement priorities and must lobby London continuously.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, regional).

% Prospective and recent immigrants whose entry is capped by 'economic absorptive capacity' quotas administered so as to avoid altering the demographic balance against the Arab majority. Individuals awaiting certificates bear the cost of a quota system justified, under this reading, as necessary to prevent involuntary displacement of the existing population rather than as a ceiling on Jewish national development.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_immigrant_communities, payer,
    moderate, biographical, constrained, regional).

% High Commissioners and the Colonial Office must administer instruments that, under this reading, subordinate the national-home clause to prior and superior obligations toward the existing population. They pay in the form of constant diplomatic and administrative friction: every land ordinance or quota decision that honors the dual-obligation reading draws Zionist protest and every concession to Zionist demand draws Arab revolt, and they cannot resolve the contradiction because they administer the same document under a second, equally textually grounded reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, agenda_setter).

% The League of Nations oversight body reviews annual mandatory reports and can question whether the administration is honoring the 'sacred trust of civilisation' and self-determination language of Article 22. It has no enforcement power beyond publicity and diplomatic pressure, but its scrutiny is the closest thing to an external check that either reading can invoke.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, diffuse).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the Mandate coordinates the transition of a non-self-governing territory toward eventual independence by binding the mandatory power to protect the existing population's civil, political, and property rights while it administers the territory — the genuine coordination problem the tutelage system was built to solve for all League mandates.
% TRANSFER_FUNCTION: The dual-obligation reading transfers legal protection and political leverage to the existing Arab population (land-tenure security, representative-government claims, immigration ceilings) at the direct expense of Zionist land acquisition, immigration volume, and demographic-parity ambitions, and at the administrative expense of British officials who must enforce restrictions the national-home clause seems to forbid.
% ABSENT_VOICES: Palestinian Arab peasants and smallholders whose tenure is nominally protected have no seat in Mandate administration or League proceedings; their protection is administered on their behalf by notables and by British officials who may or may not enforce it. Zionist settlement organizations, though present as petitioners, are structurally excluded from the reading's own premise that the national-home clause is subordinate.
% DISAPPEARANCE_RATIONALE: If the dual-obligation reading were abandoned entirely in favor of unrestricted national-home primacy, land-transfer restrictions would lift, immigration quotas keyed to demographic balance would fall away, and representative-government claims grounded in Arab majority status would lose their legal footing — the entire framework Palestinian Arab elites used to contest Zionist settlement in League and mandatory forums would disappear along with the tenancy protections fellahin depend on.
% FOUNDING_PROBLEM: The mandate system was built to reconcile League of Nations tutelage principles (Article 22's 'sacred trust,' provisional recognition of independence for Class A mandates like Palestine) with the prior British commitment in the Balfour Declaration, given an explicit textual proviso that nothing should prejudice the civil and religious rights of existing non-Jewish communities.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian Arab political bodies and Arab members of the 1930 and 1937 commissions of inquiry (Shaw, Peel) attest that the protective obligation was real and was being violated in practice — corroboration from outside the beneficiary group comes from British-appointed inquiry commissions themselves, which repeatedly found land dispossession and demographic anxiety substantiated, even though the same commissions ultimately recommended partition or continued national-home policy rather than full protective enforcement.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 by 1939) because, from this reading's own standpoint, the arrangement is not merely protective but actively redistributive: it forecloses Zionist land acquisition and immigration volume as a matter of legal obligation, and that foreclosure intensifies through the interval as land ordinances (1920, 1921 Haycraft, 1930 Passfield White Paper, 1939 White Paper) tighten. Suppression rises in step (0.50 to 0.71) because enforcing the dual-obligation reading against sustained Zionist and British Colonial Office pressure requires increasing administrative coercion — restrictive ordinances, quota administration, and eventually the 1939 White Paper's near-total land-transfer ban. Theater ratio is moderate and rising (0.25 to 0.45): a meaningful share of the 'protective' apparatus (commissions of inquiry that recommend but do not bind, land ordinances poorly enforced against evasion) is performative relative to the sustained material harm of dispossession that inquiry after inquiry documents but does not halt. All three metrics share one time grid across 1920-1939.
 *
 * PERSPECTIVAL GAP:
 *   From the Arab beneficiary seats, the arrangement should compute as closer to a rope with the coordination function genuinely operative when enforced. From the Zionist payer seats, the identical clauses compute as suppression of a competing, equally textual right. From the British administrator seat, the arrangement computes as an unresolvable tangled rope: coordinating a transition to self-government while simultaneously bound to a national-home clause that its own protective reading limits. The engine computes these divergent seat-level types from the structural power/exit/scope data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab landholders, elites, and peasants are declared beneficiaries and sit toward the low-d end: the instruments, on this reading, subsidize their tenure and political claims. Zionist organizations and Jewish immigrant communities are declared victims and sit toward the high-d end: the same clauses that protect Arab tenure operate as caps on their acquisition and entry. British administrators are dual-positioned — institutionally powerful but structurally squeezed, paying in political and administrative cost even though they hold the enforcement pen; this is why they carry both agenda_setter and payer roles rather than being cleanly one or the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling tutelage/self-determination principles with a prior national-home commitment) is authored as contested rather than resolved: Arab leadership and repeated British-appointed commissions (Shaw 1930, Peel 1937) attest the protective obligation remains live and violated, while the mandatory power's actual land and immigration policy through most of the interval tracked national-home primacy more than protective obligation. Reading the founding-problem status as contested rather than dead prevents mislabeling this as either pure extraction (ignoring that real, if underenforced, protective machinery existed) or pure coordination (ignoring that the same machinery was chronically under-enforced against the population it was meant to protect).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_priority_of_proviso_clause,
    'Does the Mandate instrument''s proviso (''nothing shall be done which may prejudice the civil and religious rights of existing non-Jewish communities'') establish a legally superior or merely coordinate obligation relative to the national-home clause?',
    'Comparative textual and drafting-history analysis of the Mandate instrument and Covenant Article 22, cross-referenced against Permanent Mandates Commission interpretive practice and international-law scholarship contemporaneous with the interval.',
    'If superior, this reading''s dual-obligation framing is the legally correct one and the jewish_national_home_primacy reading is the deviant administrative practice; if merely coordinate, the two readings are genuinely undecidable from the text alone and the operative constraint shifts toward mandatory_interpretive_discretion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_priority_of_proviso_clause, conceptual, 'Whether the protective proviso is textually superior to or merely coordinate with the national-home clause.').

omega_variable(
    enforcement_gap_measurement,
    'To what extent did British administrators actually enforce land-transfer and tenancy protections relative to the volume of documented dispossession over the interval?',
    'Quantitative analysis of land ordinance enforcement records, eviction litigation outcomes, and Peel/Shaw Commission dispossession estimates against total land transactions 1920-1939.',
    'A wide enforcement gap would support classifying this reading''s protective machinery as substantially theatrical (raising theater_ratio further); a narrow gap would support treating the protections as materially operative coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_measurement, empirical, 'Whether protective enforcement matched protective legal language in practice.').

omega_variable(
    sibling_reading_selection_pressure,
    'Is the choice to author this as the primary reading (rather than jewish_national_home_primacy) itself a reflection of which reading later historiography treats as more textually grounded, or of contemporary political sympathies of the generating process?',
    'Cross-check against primary Mandate drafting history (Balfour Declaration correspondence, San Remo conference records, Churchill White Paper 1922) independent of retrospective historiographical framing.',
    'If the drafting history shows the national-home clause was always intended as primary with the proviso as a limiting afterthought, this reading''s epsilon and beneficiary structure should be understood as the contested, minority legal position rather than a coordinate reading of equal textual weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Whether framing this reading as coordinate rather than subordinate reflects the drafting record or retrospective selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(balf_tr_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1923, 0.3).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.38).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1933, 0.42).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.44).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.45).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(balf_be_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.68).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1933, 0.73).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.77).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(balf_su_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1923, 0.55).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.63).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1933, 0.66).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.7).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language 'Balfour Mandate' kernel per the ε-invariance principle. dual_obligation_indigenous_rights (this story) authors high epsilon tangled_rope with Palestinian Arab elites/communities as beneficiaries and Zionist organizations/British administrators as victims/payers. jewish_national_home_primacy authors a structurally distinct high-epsilon reading with inverted beneficiary/victim sets (Zionist organizations as beneficiaries, Palestinian Arab communities as victims). mandatory_interpretive_discretion authors a third constraint locating the operative extraction in unreviewable British administrative discretion itself rather than in either substantive textual reading, with British administrators as agenda-setters over both other readings' populations. All three share the same underlying Mandate text but are not the same constraint under the ε-invariance test — measuring 'the Mandate' by which population's rights are foregrounded yields different epsilon values, hence three files, not one with a parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
