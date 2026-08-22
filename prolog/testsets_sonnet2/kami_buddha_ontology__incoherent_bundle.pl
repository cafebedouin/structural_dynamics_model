% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This story instantiates the 'incoherent_bundle' reading of the
 *   kami_buddha_ontology kernel. Rather than adjudicating whether kami and
 *   buddhas are ontologically identical (the honji_suijaku_monism reading) or
 *   ontologically distinct domain-partitioned entities (the domain_partition
 *   reading), this reading treats shinbutsu-shugo itself as never having
 *   achieved a single stable ontology. The bundle survives roughly a
 *   millennium (Nara period combinatory practice through the Meiji-era
 *   shinbutsu bunri separation edicts and their partial failure) not because
 *   any framework resolved the fusion/separation tension but because the
 *   institutions administering it — jingu-ji complexes, yamabushi and
 *   shugenja ritual specialists — profit from the flexibility that unresolved
 *   contradiction provides. The theater_ratio rises over the interval as
 *   combinatory ritual specialization becomes increasingly professionalized
 *   and codified in forms (mandala diagrams, ritual manuals, honji-suijaku
 *   lineage charts) whose theoretical content masks rather than resolves the
 *   underlying incoherence. The dip at t=1000 in base_extractiveness reflects
 *   the temporary disruption of the Meiji separation edicts, which forcibly
 *   cut some combinatory institutions but did not resolve the ontological
 *   question — extraction resumes and climbs afterward as new syncretic and
 *   revivalist forms reconstitute around the same unresolved bundle.
 *
 * KEY AGENTS:
 *   - syncretic_temple_shrine_complexes: primary institutional beneficiary and agenda-setter — administers the bundle without needing to resolve it
 *   - combinatory_ritual_specialists: professional beneficiary whose expertise depends on the bundle's contradictions persisting
 *   - systematic_theologians: payer — labor toward resolution gets absorbed rather than adopted
 *   - lay_practitioners_seeking_doctrinal_clarity: primary payer — bears the cognitive cost of inconsistent answers
 *   - meiji_era_separation_edicts_administrators: excluded — attempted forced resolution from outside the bundle's own logic, largely failed to produce coherence
 *   - comparative_religion_scholars: analytical observer — documents the absence of a stable resolution across the historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.5).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, piton).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '5208e684-3353-419e-878a-e563c416273e').
narrative_ontology:cs_kernel_codification('5208e684-3353-419e-878a-e563c416273e', distributed).
narrative_ontology:cs_authority_grounding('5208e684-3353-419e-878a-e563c416273e', practice).
narrative_ontology:cs_interpretation_layer_present('5208e684-3353-419e-878a-e563c416273e').
narrative_ontology:cs_reading_relation('5208e684-3353-419e-878a-e563c416273e', kami_buddha_ontology__honji_suijaku_monism, influences).
narrative_ontology:cs_reading_relation('5208e684-3353-419e-878a-e563c416273e', kami_buddha_ontology__domain_partition, influences).
narrative_ontology:cs_axiom('5208e684-3353-419e-878a-e563c416273e', foundational, ontological_settlement_never_achieved).
narrative_ontology:cs_axiom_status(ontological_settlement_never_achieved, holdable).
narrative_ontology:cs_axiom_grounding('5208e684-3353-419e-878a-e563c416273e', ontological_settlement_never_achieved, empirically_contingent).
narrative_ontology:cs_axiom('5208e684-3353-419e-878a-e563c416273e', foundational, ritual_efficacy_independent_of_doctrinal_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_independent_of_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('5208e684-3353-419e-878a-e563c416273e', ritual_efficacy_independent_of_doctrinal_coherence, instrumental).
narrative_ontology:cs_reference_frame('5208e684-3353-419e-878a-e563c416273e', nara_period_combinatory_practice_onset).
narrative_ontology:cs_drift_state('5208e684-3353-419e-878a-e563c416273e', meiji_separation_and_aftermath, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5208e684-3353-419e-878a-e563c416273e', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, syncretic_temple_shrine_complexes).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, combinatory_ritual_specialists).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, systematic_theologians).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_independent_of_ontological_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jingu-ji and shrine-temple complexes administer combined kami-buddha ritual calendars, land holdings, and pilgrimage economies. They draw legitimacy and revenue from BOTH fusion claims (honji suijaku identifications that let a shrine host Buddhist rites) and separation claims (kami purity rules that justify distinct shrine precincts and personnel) depending on which framing serves the occasion. Institutional continuity, not doctrinal consistency, is what they actually manage.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, syncretic_temple_shrine_complexes, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, syncretic_temple_shrine_complexes, agenda_setter).

% Yamabushi, shugenja, and shrine-temple priests hold specialized knowledge of which combinatory formula (identity, hierarchy, reciprocity) applies to which rite. Their professional standing depends on the bundle's internal contradictions remaining unresolved and un-simplified — a coherent single ontology would make much of their interpretive expertise redundant.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, combinatory_ritual_specialists, beneficiary,
    organized, generational, constrained, regional).

% Scholars and doctrinally-minded clerics (including later Shinto revivalists like Hirata Atsutane's intellectual predecessors) who attempt to systematize the kami-buddha relationship into a single consistent doctrine repeatedly find their frameworks absorbed, ignored, or selectively cited by institutions that need the ambiguity preserved. Their labor produces texts that get treated as one more resource in the bundle rather than a resolution of it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, systematic_theologians, payer,
    moderate, biographical, constrained, national).

% Ordinary worshippers who ask straightforward questions (is this kami a buddha or not? which rites are for which needs?) receive answers that shift by context, ritual specialist, and site, without acknowledgment that the tradition itself is bundling incompatible commitments. They bear the cognitive and practical cost of navigating contradictions that institutions have no incentive to resolve.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, constrained, local).

% The shinbutsu bunri policy administrators who attempted to forcibly separate kami and buddha worship (1868 onward) treated the bundle as if it had a clean seam that could be cut. Their attempt to impose domain_partition by fiat is not represented within the bundle's own institutional logic and largely failed to achieve doctrinal clarity even as it destroyed specific combinatory institutions — evidence for this reading, not a party the bundle itself accommodates.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, meiji_era_separation_edicts_administrators, excluded,
    institutional, generational, trapped, national).

% Academic observers who catalog the honji suijaku theories, note their internal inconsistencies across regions and periods, and document that no single systematized text or council ever settled the relationship — the historical record itself is the evidence base for the incoherent-bundle reading.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bundle allows a single ritual-institutional complex to serve multiple, incompatible practical needs — life-affirming kami festivals, death-related Buddhist funerary rites, protective combinatory deity worship — without forcing any single site or lineage to choose one ontology and lose the other's constituency, revenue, or ritual repertoire.
% TRANSFER_FUNCTION: Moves interpretive authority and ritual fee income toward institutions and specialists able to hold multiple incompatible framings simultaneously, and moves the cost of unresolved contradiction onto lay practitioners and systematizing theologians who want a single coherent answer and do not get one.
% ABSENT_VOICES: Systematic theologians whose consistent frameworks got selectively cited rather than adopted; ordinary worshippers whose direct questions about what a kami actually is get inconsistent answers depending on which specialist and site they ask; the Meiji separation administrators, whose forced-partition attempt is treated by the tradition's own institutions as an external rupture rather than as evidence the bundle was never resolvable.
% DISAPPEARANCE_RATIONALE: If the institutionally sustained ambiguity vanished and a single coherent kami-buddha ontology were adopted tomorow, jingu-ji complexes would lose the flexibility that lets them run both kami-affirming and buddha-affirming rites from the same site; combinatory ritual specialists would lose the professional niche of adjudicating which formula applies where; and lay practitioners would finally get consistent answers, at the cost of losing ritual options the ambiguity currently makes available (e.g. treating a kami as both locally sovereign and buddha-derived depending on the need).
% FOUNDING_PROBLEM: Early Japanese Buddhist institutions needed to establish legitimacy and ritual reach in a landscape already dense with kami cults; kami cults needed frameworks that let them absorb Buddhist ritual technology and doctrinal prestige without simply being replaced. The bundle solved both problems at once by never settling which relationship (identity, hierarchy, domain-separation) was actually true.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and historians of the honji suijaku corpus attest, from outside any single shrine-temple lineage's interests, that no textual tradition ever produced a stable resolution and that regional variation in the formulas is itself in the historical record. Meiji-era separation administrators attest (via their own policy failure and subsequent partial reversals) that the problem the bundle solved was never actually resolved by fiat, only administratively suppressed at particular sites. The beneficiary institutions themselves, by contrast, consistently narrate the relationship as settled doctrine appropriate to context — which is exactly the self-interested corroboration this field is designed to discount.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) rather than high: the bundle is not primarily a rent-extraction device but an institutional survival strategy whose costs land unevenly on those seeking clarity rather than on a clearly identifiable extracted class. Suppression is moderate (0.5) — no central authority enforces the incoherence; it persists through diffuse institutional incentive rather than active coercion, though local sites do suppress alternative systematic framings that would threaten their combinatory ritual repertoire. Theater ratio is high and rising (0.68) because an enormous apparatus of ritual manuals, honji-suijaku correspondence charts, and combinatory mandalas exists to give the appearance of systematic doctrine while the underlying relationship (identity? hierarchy? separate domains?) is never actually settled — this is the piton signature: elaborate performative maintenance around a function (doctrinal coherence) that has arguably never been achieved and may not be achievable within the bundle's own terms. Accessibility collapse is low-moderate (0.4): alternative framings (strict domain partition, strict monism) remain articulable and are periodically attempted (Meiji edicts, revivalist Shinto systematizers) but never successfully displace the bundle at the institutional level.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic institutions and ritual specialists sit near the beneficiary end: they derive professional standing, ritual flexibility, and revenue precisely from the bundle's unresolved contradictions, and their exit options (arbitrage, constrained-but-flexible) let them deploy whichever framing serves a given occasion. Systematic theologians and lay practitioners sit nearer the target end: they bear the cost of incoherence (theologians' resolving labor gets absorbed without adoption; practitioners get inconsistent answers) without comparable capacity to exit into a clarified alternative — the alternative institutions barely exist at meaningful scale within the tradition. The Meiji administrators are excluded rather than positioned within the bundle's directionality at all, because their forced-separation project operated from outside the bundle's own institutional logic; their failure to fully resolve the ontology is evidence for this reading, not a party the bundle accommodates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (simultaneous need for Buddhist institutions to gain purchase in a kami-dense landscape and for kami cults to absorb Buddhist prestige without erasure) is genuinely contested as 'live' or 'dead' — modern Japan's syncretic religious landscape still exhibits functionally analogous coordination needs (shrines and temples still often coexist, festivals still blend registers), which argues the founding problem persists in modified form. But the SPECIFIC unresolved ontological question the bundle was built around has had ample time and scholarly attention to resolve and has not — that persistence-without-resolution, corroborated by scholars outside the beneficiary institutions, is exactly what prevents this from being read as ongoing rope-style coordination. It is better read as institutional inertia (piton) dressed in elaborate combinatory doctrine (high theater_ratio) rather than either genuine unresolved coordination-in-progress or pure extraction with identifiable victims paying a concentrated rent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundle_vs_genuine_synthesis,
    'Is shinbutsu-shugo a genuinely incoherent bundle of contradictory commitments sustained by institutional inertia, or does it instantiate a coherent (if non-Western) logical structure — e.g., a both/and or perspectival ontology — that appears incoherent only when read through a bivalent Western ontological lens?',
    'Close comparative analysis of whether the honji-suijaku, domain-partition, and reciprocal-hierarchy elements can be shown to follow a single, if unfamiliar, logical schema (e.g. graded or context-relative ontological commitment) consistently applied across regions and periods, versus documented internal contradiction even within single lineages or texts.',
    'If a coherent non-bivalent logic is demonstrated, this reading collapses toward a variant of honji_suijaku_monism (a genuine, if complex, ontology) rather than standing as a distinct incoherent-bundle reading; if internal contradiction is confirmed even within single institutional lineages, this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_vs_genuine_synthesis, conceptual, 'Whether apparent incoherence is genuine or an artifact of applying bivalent ontological standards to a non-bivalent framework.').

omega_variable(
    sibling_reading_foreclosure_location,
    'Where exactly does the disagreement between this reading and its siblings (honji_suijaku_monism, domain_partition) live — is it a disagreement about historical fact (what premodern Japanese institutions actually believed and practiced) or a disagreement about how to characterize acknowledged variation (whether variation across sites/periods constitutes ''incoherence'' or ''contextual application of one true doctrine'')?',
    'Systematic textual and institutional-history survey distinguishing cases of (a) explicit doctrinal contradiction within a single lineage''s own texts from (b) mere regional/site-level variation compatible with an underlying single doctrine differently applied.',
    'If most variation is type (b), the domain_partition or honji_suijaku_monism readings gain support as the more accurate single-ontology accounts and this reading''s extractiveness/theater framing overstates incoherence; if substantial variation is type (a), this reading''s claim of genuine institutionally-sustained contradiction is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_location, conceptual, 'Locating whether the kernel dispute is about historical fact or about characterizing acknowledged variation.').

omega_variable(
    meiji_separation_as_evidence_or_artifact,
    'Does the partial failure of the Meiji-era shinbutsu bunri separation edicts to produce a clean, stable domain-partitioned religious landscape constitute genuine evidence that the underlying bundle was never separable (supporting this reading), or was the failure primarily a product of specific 19th-century political contingencies unrelated to the deeper ontological structure?',
    'Historical analysis of the specific mechanisms of separation-edict failure (administrative capacity, local resistance, subsequent State Shinto reconstruction) versus evidence of spontaneous re-fusion at sites where separation was actually enforced successfully and durably.',
    'If durably separated sites show no drift back toward combinatory practice, the domain_partition reading is strengthened and this reading''s core evidentiary claim weakens; if even successfully separated sites show later re-fusion pressure, this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence_or_artifact, empirical, 'Whether Meiji separation''s partial failure is structural evidence for bundle-incoherence or a contingent historical accident.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 200, 0.45).
narrative_ontology:measurement_basis(kami_tr_t200, projected).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__incoherent_bundle, theater_ratio, 400, 0.55).
narrative_ontology:measurement_basis(kami_tr_t400, projected).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.6).
narrative_ontology:measurement_basis(kami_tr_t600, observed).
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__incoherent_bundle, theater_ratio, 800, 0.65).
narrative_ontology:measurement_basis(kami_tr_t800, observed).
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1000, 0.62).
narrative_ontology:measurement_basis(kami_tr_t1000, observed).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.68).
narrative_ontology:measurement_basis(kami_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 200, 0.38).
narrative_ontology:measurement_basis(kami_be_t200, projected).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 400, 0.45).
narrative_ontology:measurement_basis(kami_be_t400, projected).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.5).
narrative_ontology:measurement_basis(kami_be_t600, observed).
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 800, 0.52).
narrative_ontology:measurement_basis(kami_be_t800, observed).
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement_basis(kami_be_t1000, observed).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(kami_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__incoherent_bundle, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kami_buddha_ontology kernel. honji_suijaku_monism authors the constraint as a coherent monist ontology (kami as suijaku traces of buddha honji) with correspondingly low-to-moderate extraction attributable to a settled doctrinal hierarchy. domain_partition authors it as a coherent dualist ontology with functional domain separation (life/purity vs. death/impurity) and its own distinct beneficiary/victim structure tied to that separation. This incoherent_bundle reading denies either sibling's premise of a single settled ontology and instead authors institutional inertia and professional specialization around persistent, unresolved contradiction as the actual structural driver — its ε (0.58) and claimed piton type are not directly comparable to either sibling's ε without accounting for the different referent each reading takes (a settled monist doctrine vs. a settled dualist doctrine vs. an unresolved institutional bundle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
