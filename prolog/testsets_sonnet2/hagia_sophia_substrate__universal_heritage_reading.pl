% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Human Heritage (Museum-Era Reading)
 *   domain: cultural heritage / sovereignty / religious authority
 *
 * SUMMARY:
 *   This story instantiates the universal-heritage reading of the Hagia
 *   Sophia kernel: the claim that the site's legitimacy rests on its status
 *   as shared human cultural heritage transcending any single religious or
 *   national claim. Under this reading, the 1934 museum conversion and the
 *   ongoing international heritage apparatus (UNESCO designation, secular
 *   Turkish state administration, global tourism/scholarship access) are the
 *   constraint's institutional form. The reading names its own beneficiaries
 *   (global tourism, international scholarship, the secularist Turkish
 *   establishment) and its own victims (the local Muslim worshipping
 *   community and Ottoman waqf trustees, whose worship claim was
 *   administratively suspended). Extraction rises over the interval as
 *   tourism monetization intensified through the late 20th century while the
 *   underlying religious grievance was never resolved, only administratively
 *   bracketed — the theater_ratio rise reflects growing performative
 *   neutrality (heritage-site staging, curated 'shared history' narrative)
 *   layered atop an increasingly contested arrangement. This is ONE of three
 *   readings of the shared kernel; the islamic_sovereignty_reading and
 *   orthodox_restitution_reading are separate constraint stories with their
 *   own ε, beneficiaries, and victims — this file does not average across
 *   them or describe their contest internally.
 *
 * KEY AGENTS:
 *   - global_tourism_sector: beneficiary (organized/arbitrage) — monetizes secular access
 *   - international_scholarship_community: beneficiary (institutional/mobile) — built careers and funding on museum-era access
 *   - secularist_turkish_elites: beneficiary/agenda_setter (institutional/constrained) — administered and symbolically depend on the museum framing
 *   - unesco_heritage_apparatus: agenda_setter/observer (institutional/analytical) — adjudicates universal-value status from outside local costs
 *   - local_muslim_worshippers: payer (powerless/trapped) — barred from worship under the neutral framing
 *   - religious_endowment_trustees: payer (moderate/constrained) — waqf claim suspended by state decree
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.6).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Human Heritage (Museum-Era Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural heritage / sovereignty / religious authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '05206566-f395-44c6-8a31-4aa5a7667e6c').
narrative_ontology:cs_kernel_codification('05206566-f395-44c6-8a31-4aa5a7667e6c', distributed).
narrative_ontology:cs_authority_grounding('05206566-f395-44c6-8a31-4aa5a7667e6c', extraction).
narrative_ontology:cs_interpretation_layer_present('05206566-f395-44c6-8a31-4aa5a7667e6c').
narrative_ontology:cs_reading_relation('05206566-f395-44c6-8a31-4aa5a7667e6c', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('05206566-f395-44c6-8a31-4aa5a7667e6c', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('05206566-f395-44c6-8a31-4aa5a7667e6c', foundational, no_single_confession_may_claim_exclusive_custody).
narrative_ontology:cs_axiom_status(no_single_confession_may_claim_exclusive_custody, holdable).
narrative_ontology:cs_axiom_grounding('05206566-f395-44c6-8a31-4aa5a7667e6c', no_single_confession_may_claim_exclusive_custody, conventional).
narrative_ontology:cs_axiom('05206566-f395-44c6-8a31-4aa5a7667e6c', secondary, secular_technocratic_administration_transcends_sectarian_claim).
narrative_ontology:cs_axiom_status(secular_technocratic_administration_transcends_sectarian_claim, overridden).
narrative_ontology:cs_axiom_grounding('05206566-f395-44c6-8a31-4aa5a7667e6c', secular_technocratic_administration_transcends_sectarian_claim, instrumental).
narrative_ontology:cs_reference_frame('05206566-f395-44c6-8a31-4aa5a7667e6c', id_1934_secular_museum_decree).
narrative_ontology:cs_drift_state('05206566-f395-44c6-8a31-4aa5a7667e6c', post_2020_reconversion, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('05206566-f395-44c6-8a31-4aa5a7667e6c', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholarship_community).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, local_muslim_worshippers).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, religious_endowment_trustees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tour operators, airlines, and hospitality businesses monetize the site as a secular monument accessible to all visitors regardless of faith. Revenue depends on the site remaining a museum with unrestricted, ticketed, photographable access rather than an active mosque with prayer-time closures and modesty requirements. They can relocate marketing emphasis to other sites if the arrangement changes, but currently capture substantial value from the universal-heritage framing.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, arbitrage, global).

% Byzantinists, art historians, and conservation scientists gained decades of relatively unrestricted access to mosaics and structural elements under museum administration. Their scholarly authority and funding streams (grants, publications, conservation contracts) are built on the site's designation as a heritage object of universal scientific interest rather than a functioning place of worship.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholarship_community, beneficiary,
    institutional, generational, mobile, global).

% Kemalist bureaucratic and cultural establishment figures who administered the 1934 museum conversion as a signature act of secular modernization, converting a contested religious site into neutral state patrimony. Their political and ideological legitimacy is tied to the museum framing; they lose symbolic capital when the site is re-converted to active mosque use, but their institutional exit is constrained by shifts in domestic electoral power.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter).

% The World Heritage Committee and affiliated technocratic bodies adjudicate conservation standards and issue statements when the site's status changes, treating the site as belonging to a trans-national inventory of universal value. They administer standards and issue rulings but bear none of the local costs of restricted worship access; their leverage is soft (reputational, funding-conditional) rather than coercive.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus, observer).

% Devout residents and pilgrims for whom the site is a former imperial mosque with unbroken religious significance. Under the museum-era universal-heritage framing, they were barred from praying inside the structure, required to treat it as a secular exhibit, and had their religious claim subordinated to a cosmopolitan narrative administered by people who do not share their stake. They have no meaningful alternative site of equivalent significance.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, local_muslim_worshippers, payer,
    powerless, biographical, trapped, local).

% Custodians of the original Ottoman waqf (religious endowment) that funded and maintained the building as a mosque for nearly five centuries. Under the universal-heritage/museum framing, the endowment's legal and religious claim over the building's use was suspended by state decree; the trustees could litigate or lobby but could not unilaterally restore worship use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, religious_endowment_trustees, payer,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes the site from exclusive religious or national custody and places it under technocratic, internationally legible stewardship, enabling conservation funding, cross-border tourism revenue, and scholarly access that a single confessional or sovereign claim might not sustain or might restrict.
% TRANSFER_FUNCTION: Moves religious access and communal ownership away from the local Muslim worshipping community and the Ottoman-era waqf trustees, and moves symbolic capital, tourism revenue, and scholarly authority toward the international heritage-tourism-scholarship complex and the secularist domestic elite that administers the site.
% ABSENT_VOICES: Local worshippers and waqf trustees were not seated at the tables (UNESCO committees, national cultural ministries, international heritage conferences) where the universal-heritage designation was formulated and defended; their objection — that the site's Islamic character predates and outweighs any claim to religious neutrality — is rarely represented inside the institutions that administer the museum framing.
% DISAPPEARANCE_RATIONALE: If the universal-heritage framing disappeared overnight, the tourism and scholarship apparatus would lose its primary legitimating narrative and much of its unrestricted access, and secularist elites would lose a flagship symbol of Kemalist modernity — a real rearrangement. But local worshippers and endowment trustees would say the world was already effectively unchanged for them under this reading, since it never recognized their claim as primary; from their seat its disappearance restores rather than disrupts the prior order. The verdict itself is disputed along the same lines as the underlying kernel.
% FOUNDING_PROBLEM: In the early Turkish Republic, the building's status as a contested former Byzantine cathedral and Ottoman imperial mosque was a live flashpoint for both Christian-minority grievance and pan-Islamic sentiment; converting it to a museum was framed as removing the site from confessional contest entirely and placing it in a neutral, scientifically administered category.
% FOUNDING_PROBLEM_CORROBORATION: International heritage bodies and academic historians outside Turkey (art historians, UNESCO documentation, foreign diplomatic archives from the 1930s) corroborate that removing confessional contest was a genuine stated goal of the 1934 decree. However, Turkish religious authorities, waqf trustees, and a substantial domestic constituency contest that the 'neutral' framing was ever neutral in practice — they attest it functioned as an active suppression of the site's Islamic character rather than a transcendence of confessional claims, and note the same secularist administration that authored the decree was also its principal beneficiary.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects substantial value capture by tourism, scholarship, and secular-nationalist symbolic politics riding on a framing that displaces a live religious claim. Suppression (0.6) is set below extreme because the suppression here is legal-administrative (decree-based exclusion from worship) rather than violent, but it is real and enforced. Theater ratio (0.52) is elevated because a significant share of the 'shared heritage' presentation — curated signage, festival programming, diplomatic photo-ops — functions as legitimating performance for an arrangement whose underlying religious dispute was never actually resolved, only suspended. Accessibility collapse (0.5) is moderate: alternative framings (worship restoration, ecclesiastical return) remained conceptually available and periodically resurfaced, so the universal-heritage framing did not achieve mountain-grade closure of alternatives. Resistance (0.7) is high, reflecting decades of domestic religious and political pressure culminating in the 2020 reconversion to mosque status — direct evidence the universal-heritage reading's authority was actively contested, not settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Global tourism, scholarship, and the secularist administrative elite sit near the beneficiary end of directionality: they collect (revenue, access, symbolic legitimacy) without bearing the framing's core cost. UNESCO sits as an agenda-setting observer with soft leverage and no local exposure — analytical exit options. Local worshippers are trapped: the site is singular and irreplaceable to their religious practice, so their d sits near the full-target end despite their formal 'equal access' as museum visitors. Endowment trustees have constrained exit — legal and political channels exist but were foreclosed for decades by state decree, keeping their d elevated but not maximal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — removing the site from confessional contest to prevent inter-communal flashpoint — was genuinely live in 1934. By the measurement interval's end, Turkish domestic religious demography and politics had shifted such that the 'neutral' framing was widely read domestically as itself a partisan secularist imposition rather than a neutral solution: the founding problem's status is contested rather than settled, which is exactly the founding_problem_status/disappearance_verdict mismatch this constraint's genealogy interview is built to surface. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (international conservation funding, cross-border scholarly access, avoidance of exclusive confessional custody) that the museum framing did provide, while still registering the asymmetric extraction from the excluded worshipping community — a pure snare framing would erase the real coordination benefit captured by scholarship and conservation; a pure rope framing would erase the suppressed worship claim. Tangled rope holds both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_or_suppression_framing,
    'Is the universal-heritage designation a genuine transcendence of confessional claims, or is ''neutrality'' itself a substantive secularist position that suppresses the site''s dominant historical-religious character (Ottoman Islamic use, 1453-1934)?',
    'Comparative analysis of how the museum-era administration allocated access, funding, and narrative emphasis relative to the site''s five-century continuous function as a mosque versus its prior 900-year function as a cathedral; examine whether ''shared heritage'' framing was applied symmetrically or disproportionately displaced the more recent and locally dominant claim.',
    'If neutrality functioned as suppression, the tangled_rope classification understates victimhood and the arrangement is closer to a snare wearing coordination cover; if genuine transcendence, the tangled_rope''s coordination half is stronger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_or_suppression_framing, conceptual, 'Whether declared neutrality is substantive suppression or genuine transcendence of confessional claim.').

omega_variable(
    kernel_framing_choice,
    'Is the correct unit of analysis the single physical site under contested legitimacy claims (the kernel framing used here), or three genuinely independent constraints that happen to share a location but have no structural interdependence?',
    'Examine whether a change in one reading''s institutional standing (e.g., the 2020 mosque reconversion) causally altered the material conditions of the other readings (tourism access, scholarly access, ecclesiastical claims) — interdependence would support the shared-kernel framing; independence would support fully decomposed unrelated constraints.',
    'The 2020 reconversion materially reduced global_tourism_sector and international_scholarship_community access and elevated islamic_sovereignty_reading''s practical authority — this observed interdependence is the evidentiary basis for treating these as siblings of one kernel rather than three unrelated constraints. If future evidence showed no such interdependence, the kernel framing itself would be miscalibrated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the three readings genuinely share one contested kernel or are independent constraints coincidentally co-located.').

omega_variable(
    unesco_leverage_extent,
    'How much actual coercive or resource-conditional leverage does the international heritage apparatus hold over Turkish domestic sovereignty decisions regarding the site, versus purely reputational/soft influence?',
    'Trace UNESCO and international body responses to the 2020 reconversion — funding threats, delisting proceedings, diplomatic pressure — and their actual material consequences for Turkish state behavior.',
    'If leverage is negligible, unesco_heritage_apparatus''s agenda_setter role is largely symbolic and the true enforcement of the universal-heritage framing rested entirely on domestic secularist political power, not international pressure — this would shift the enforcement burden more heavily onto secularist_turkish_elites in any recomputation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unesco_leverage_extent, empirical, 'The real versus nominal enforcement power of international heritage bodies over site status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hagi_tr_t15, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(hagi_tr_t30, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(hagi_tr_t45, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement(hagi_tr_t75, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 75, 0.49).
narrative_ontology:measurement(hagi_tr_t90, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 90, 0.52).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hagi_be_t15, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(hagi_be_t30, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(hagi_be_t45, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(hagi_be_t75, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(hagi_be_t90, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hagi_su_t15, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(hagi_su_t30, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(hagi_su_t45, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(hagi_su_t75, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(hagi_su_t90, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 90, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.1).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hagia_sophia_substrate kernel, each a structurally distinct constraint per the ε-invariance principle: universal_heritage_reading (this file, tangled_rope, ε=0.68, technocratic secular authority), islamic_sovereignty_reading (Islamic worship sovereignty grounded in conquest/waqf, distinct authority and beneficiary set), and orthodox_restitution_reading (Byzantine ecclesiastical restitution claim, distinct authority and beneficiary set). Each carries its own ε, beneficiaries, victims, and classification; they are linked here for contamination-propagation analysis, not averaged into one verdict. The 2020 reconversion event is the empirical hinge showing these readings are causally interdependent rather than merely thematically related.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
