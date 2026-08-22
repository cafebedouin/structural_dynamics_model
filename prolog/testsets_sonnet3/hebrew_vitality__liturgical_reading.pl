% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Liturgical Continuity as Constitutive Vitality of Hebrew
 *   domain: sociolinguistics/religious_studies
 *
 * SUMMARY:
 *   This story instantiates the liturgical reading of the contested 'Hebrew
 *   vitality' kernel: the claim that unbroken ritual and liturgical use of
 *   Hebrew across the diaspora period constitutes the language's vitality,
 *   full stop — that recitation IS life, not merely a holding pattern
 *   awaiting revival. This is a low-extraction coordination story: rabbinic
 *   authorities and liturgical communities benefit from an internally
 *   coherent, low-cost claim to continuous linguistic-religious identity, and
 *   no group bears an identifiable structural cost from the claim's
 *   operation. The reading does not address, endorse, or require the
 *   vernacular-revival narrative (native_daily_reading) or the two-stage
 *   substrate-plus-reconstruction narrative (hybrid_continuity_reading);
 *   those are separate constraints with their own ε and stakeholder
 *   structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.22).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.28).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Continuity as Constitutive Vitality of Hebrew").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '45e7e036-a480-4085-a2e3-f2a31656f716').
narrative_ontology:cs_kernel_codification('45e7e036-a480-4085-a2e3-f2a31656f716', fixed_text).
narrative_ontology:cs_authority_grounding('45e7e036-a480-4085-a2e3-f2a31656f716', lineage).
narrative_ontology:cs_interpretation_layer_present('45e7e036-a480-4085-a2e3-f2a31656f716').
narrative_ontology:cs_reading_relation('45e7e036-a480-4085-a2e3-f2a31656f716', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('45e7e036-a480-4085-a2e3-f2a31656f716', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('45e7e036-a480-4085-a2e3-f2a31656f716', foundational, ritual_recitation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(ritual_recitation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('45e7e036-a480-4085-a2e3-f2a31656f716', ritual_recitation_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('45e7e036-a480-4085-a2e3-f2a31656f716', secondary, generative_vernacular_production_not_required_for_vitality).
narrative_ontology:cs_axiom_status(generative_vernacular_production_not_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('45e7e036-a480-4085-a2e3-f2a31656f716', generative_vernacular_production_not_required_for_vitality, conventional).
narrative_ontology:cs_reference_frame('45e7e036-a480-4085-a2e3-f2a31656f716', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('45e7e036-a480-4085-a2e3-f2a31656f716', post_israeli_vernacular_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45e7e036-a480-4085-a2e3-f2a31656f716', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, unbroken_ritual_use_constitutes_language_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret the liturgical corpus, determine correct recitation, and certify continuity of practice across diaspora communities. Their institutional standing depends on Hebrew's status as a living sacral language transmitted through unbroken ritual use; they set the terms by which 'vitality' is recognized within the tradition.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Synagogue congregations and prayer communities who recite, chant, and study Hebrew liturgy as a matter of religious practice, without necessarily speaking Hebrew as a vernacular. They gain continuity of identity and access to sacred text through this practice; participation is voluntary and communities elsewhere maintain other liturgical languages without penalty.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_communities, beneficiary,
    organized, generational, mobile, global).

% Sociolinguists studying language vitality who would object that recitation of fixed liturgical text without generative, spontaneous production does not meet standard vitality criteria (native acquisition, everyday communicative use). Their disciplinary framework is not represented in the liturgical reading's internal criteria for what counts as 'alive.'
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, hebraist_linguists, excluded,
    moderate, biographical, analytical, global).

% Individuals learning Hebrew for religious or cultural reasons, who encounter the liturgical reading as the baseline claim that their tradition's language was never dead. They neither pay a cost nor extract a benefit from the classification itself; their learning proceeds regardless of which reading is authoritative.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, diaspora_hebrew_learners, observer,
    powerless, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared criterion — unbroken ritual recitation — by which a religious and cultural community can claim continuous possession of its sacred language across two millennia of dispersion, without requiring that the language have been anyone's mother tongue in the interim.
% TRANSFER_FUNCTION: Moves interpretive authority over what counts as linguistic 'life' to those who administer liturgical practice (rabbinic authorities), rather than to linguists, native speakers, or state language-planning bodies. No material transfer occurs; the transfer is one of definitional authority.
% ABSENT_VOICES: Sociolinguists and vernacular-revival historians who hold that recitation without generative production is preservation, not vitality, are not party to the liturgical community's internal framework — their objection surfaces only in the sibling native_daily_reading, not here.
% DISAPPEARANCE_RATIONALE: If the liturgical-continuity claim were abandoned, the religious practice of Hebrew recitation would likely continue unchanged materially, but the communities' self-understanding of an unbroken linguistic-religious identity spanning exile and return would lose its primary warrant — some communities would experience this as a rupture, others (favoring the native_daily_reading) would view it as merely correcting an overclaim.
% FOUNDING_PROBLEM: Following the loss of Hebrew as a majority vernacular after antiquity, communities needed a warrant for claiming continuous ownership of their sacred and communal language despite not speaking it day to day, and for justifying the labor of teaching and reciting it across generations of dispersion.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and liturgical communities attest the founding problem remains live: unbroken transmission is still the operative warrant for religious practice. Historical sociolinguists (e.g., scholars of the modern Hebrew revival) attest from outside the beneficiary set that the vernacular-vitality problem this reading answers was effectively superseded once native Hebrew speech re-emerged in the 19th-20th century, making the liturgical claim function now as identity-continuity narrative rather than an active linguistic-survival warrant.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the liturgical-continuity claim does not require anyone to pay a material or opportunity cost to sustain it — communities recite by choice, and no enforcement apparatus compels adherence outside voluntary religious practice. Suppression (0.28) reflects mild internal social pressure within observant communities to accept the continuity narrative rather than the more linguistically rigorous native-generation standard, but this pressure is normative, not coercive. Accessibility collapse is moderate (0.4): alternative framings (vernacular revival narratives) are readily available in academic and even popular discourse, so the liturgical claim has not foreclosed its alternatives, it merely coexists with them within different institutional domains.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities are the clearest beneficiaries: their interpretive authority over what counts as linguistic vitality is the very thing the reading vindicates, and they administer the practices that instantiate it (agenda_setter + beneficiary). Liturgical communities benefit through identity continuity without material extraction. No victim group is declared because preservation, as this reading defines it, imposes no cost on any party — this is consistent with the expected structural delta for this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The liturgical reading's founding problem (warranting continuous possession of a sacred language absent vernacular speech) could be read as having been rendered obsolete by the actual 19th-20th century vernacular revival — once Hebrew became a native daily language in Israel, the liturgical-only warrant was no longer functionally necessary to defend Hebrew's survival. But the reading persists because it answers a different, still-live question: religious-communal identity continuity for diaspora and non-Israeli communities who do not speak Hebrew natively. Classifying this as rope (rather than snare or piton) prevents mislabeling this low-cost identity-coordination function as extraction; it also resists inflating a genealogically-superseded survival warrant into present-day extraction, since no concentrated beneficiary captures material rents from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_ambiguity,
    'Does unbroken liturgical/ritual recitation of a language, absent generative vernacular use, constitute ''vitality'' in the same sense linguists apply to living languages, or is this a category distinct from vitality (preservation, ritual persistence) that the liturgical reading has relabeled?',
    'Comparative sociolinguistic analysis against standard vitality criteria (intergenerational transmission, spontaneous native production, everyday communicative domains) applied consistently across liturgical-only languages (e.g., Sanskrit, Ge''ez, Church Latin) versus Hebrew''s actual pre-revival status.',
    'If liturgical recitation categorically fails standard vitality criteria, this reading''s core claim is better understood as a religious-identity claim wearing linguistic vocabulary — which would not change its low ε but would sharpen the omega about which discourse community''s criteria govern the label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_ambiguity, conceptual, 'Whether ritual recitation meets the definitional bar for ''language vitality'' or constitutes a distinct category the reading has relabeled.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (liturgical, native_daily, hybrid_continuity) diverge — on the definition of ''vitality'' itself, on the empirical historical record of Hebrew''s usage between antiquity and the 19th century, or on which discourse community''s authority should adjudicate the question?',
    'Structural comparison of the three sibling constraint stories'' axioms and beneficiary/victim declarations — if the divergence is definitional, axioms will differ (grounding_type conventional vs empirically_contingent); if empirical, the readings should converge once historical evidence is fixed; if authority-based, the disagreement is irreducible across discourse communities.',
    'Determines whether the kernel readings can ever be reconciled by evidence (empirical disagreement) or represent permanently coexisting normative frameworks (definitional/authority disagreement) — this bears on whether ''forecloses'' or ''coexists_with'' is the correct relation in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating whether the kernel''s sibling readings disagree definitionally, empirically, or over adjudicating authority.').

omega_variable(
    beneficiary_capture_of_definitional_authority,
    'Is the liturgical reading a genuine, low-cost coordination mechanism for religious-communal identity, or does it function to insulate rabbinic institutional authority from the more demanding (and potentially destabilizing) native-generation vitality standard that secular Hebrew revival scholarship applies?',
    'Examine whether rabbinic authorities have historically resisted or downplayed vernacular Hebrew revival narratives that might diminish liturgical Hebrew''s special status, versus whether they have embraced hybrid narratives without institutional friction.',
    'If resistance is documented, the low-ε rope classification may understate a soft form of definitional extraction (protecting institutional relevance); if no resistance is found, the rope classification is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_definitional_authority, empirical, 'Whether the liturgical reading''s low extraction understates institutional self-protection by rabbinic authorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__liturgical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the colloquial claim 'Hebrew was/wasn't a living language before its modern revival.' Each reading fixes a different referent for what 'vitality' means and therefore carries a different ε: liturgical_reading (this story) has the lowest ε (~0.22, pure voluntary religious coordination, no victims); native_daily_reading is expected to carry different beneficiary/victim structure keyed to the vernacular-revival narrative's own contested claims (e.g., displacement of Yiddish/Arabic-Jewish vernaculars during the Israeli revival); hybrid_continuity_reading synthesizes both, treating liturgical preservation as necessary substrate and vernacular reconstruction as the additional required act. Per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
