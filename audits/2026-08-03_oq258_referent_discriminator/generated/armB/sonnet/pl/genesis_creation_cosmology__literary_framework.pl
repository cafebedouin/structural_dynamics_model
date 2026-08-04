% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__literary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__literary_framework, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: genesis_creation_cosmology__literary_framework
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Literary Framework (Non-Cosmological Reading)
 *   domain: religious_studies/theology/philosophy_of_science
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Genesis 1-2 creation-account
 *   kernel: the literary-framework reading, which holds that the text deploys
 *   the shared cosmological schema of the Ancient Near East (a solid
 *   firmament, waters above and below, a flat disc-earth) as inherited
 *   literary furniture, not as an assertion about the physical structure of
 *   the cosmos. This reading displaces BOTH the young-earth literalist
 *   authority claim (the text is not making historical-scientific assertions
 *   to be defended against geology and cosmology) AND, more subtly, the
 *   theistic-evolution accommodation claim (the text is not even making
 *   theological truth-claims coded in figurative language that map onto
 *   modern cosmological categories) — instead the text is read as a
 *   cultural-literary artifact whose ANE cosmological furniture is incidental
 *   to its rhetorical and theological purposes (polemic against
 *   Babylonian/Egyptian cosmogonies, articulation of a single sovereign
 *   deity, sabbath etiology), decoupled from any claim, literal or
 *   figurative, about how the universe actually came to be. This is a
 *   narrower and more deflationary claim than theistic evolution's
 *   'non-literal but still cosmologically referential' position.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholars: primary agenda-setters who produce and circulate the reading (institutional/arbitrage)
 *   - young_earth_denominational_authorities: primary institutional payers whose authority structure depends on a historical reading (powerful/trapped)
 *   - literalist_lay_congregants: primary individual payers whose faith identity is fused with the historical reading (powerless/identity_locked)
 *   - mainline_theological_institutions and science_compatible_clergy: secondary beneficiaries who gain an accommodation strategy
 *   - comparative_religion_scholars: analytical observers documenting the reading-conflict across traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__literary_framework, 0.28).
domain_priors:suppression_score(genesis_creation_cosmology__literary_framework, 0.22).
domain_priors:theater_ratio(genesis_creation_cosmology__literary_framework, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, extractiveness, 0.28).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__literary_framework, rope).
narrative_ontology:human_readable(genesis_creation_cosmology__literary_framework, "Genesis 1-2 as Ancient Near Eastern Literary Framework (Non-Cosmological Reading)").
narrative_ontology:topic_domain(genesis_creation_cosmology__literary_framework, "religious_studies/theology/philosophy_of_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__literary_framework, '3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f').
narrative_ontology:cs_kernel_codification('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', fixed_text).
narrative_ontology:cs_authority_grounding('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', expertise).
narrative_ontology:cs_interpretation_layer_present('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f').
narrative_ontology:cs_reading_relation('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', genesis_creation_cosmology__young_earth_literal, forecloses).
narrative_ontology:cs_reading_relation('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', genesis_creation_cosmology__theistic_evolution, influences).
narrative_ontology:cs_axiom('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', foundational, text_makes_no_cosmological_claim).
narrative_ontology:cs_axiom_status(text_makes_no_cosmological_claim, holdable).
narrative_ontology:cs_axiom_grounding('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', text_makes_no_cosmological_claim, empirically_contingent).
narrative_ontology:cs_axiom('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', secondary, ane_schema_is_inherited_literary_furniture_not_belief_report).
narrative_ontology:cs_axiom_status(ane_schema_is_inherited_literary_furniture_not_belief_report, holdable).
narrative_ontology:cs_axiom_grounding('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', ane_schema_is_inherited_literary_furniture_not_belief_report, empirically_contingent).
narrative_ontology:cs_reference_frame('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', ane_comparative_philological_consensus).
narrative_ontology:cs_drift_state('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', post_dead_sea_scrolls_and_mesopotamian_archaeology_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3d0e1e8d-b0ad-4ac0-924d-c8c784a7004f', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, mainline_theological_institutions).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, science_compatible_clergy).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, young_earth_denominational_authorities).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, literalist_lay_congregants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and circulate the comparative ANE-schema reading through journals, seminaries, and commentaries. Their professional standing and publication record benefit from a reading that positions Genesis as a datable literary artifact analyzable alongside Enuma Elish and Atrahasis. They can move between confessional and secular academic contexts freely; the reading costs them little and enhances their disciplinary legitimacy.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, academic_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__literary_framework, academic_biblical_scholars, beneficiary).

% Seminaries and denominations that have already accommodated modern cosmology adopt this reading to resolve the apparent science-scripture conflict without abandoning scriptural authority altogether. It lets them retain institutional continuity and credibility with educated congregants and the broader academy.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, mainline_theological_institutions, beneficiary,
    institutional, generational, mobile, national).

% Pastors and educators who need a framework letting them teach evolutionary biology and cosmology in Sunday school without contradicting the text they preach from. Adopting the literary-framework reading resolves personal and pastoral tension but requires distancing themselves from literalist peers, which can cost them standing within their own denominational networks.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, science_compatible_clergy, beneficiary,
    moderate, biographical, constrained, regional).

% Denominational bodies whose doctrinal statements, seminary curricula, and institutional identity depend on Genesis functioning as a historical and cosmological record. The literary-framework reading, if accepted broadly, erodes their claim to be defending 'what the text plainly says' and undermines decades of institution-building (creation science organizations, statements of faith, credentialing requirements). They cannot simply adopt the rival reading without dismantling their own authority structure.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, young_earth_denominational_authorities, payer,
    powerful, generational, trapped, national).

% Ordinary believers for whom a historical, cosmologically-informative Genesis is fused with their sense of scriptural reliability and personal faith identity. Being told the text they were taught to read as historical record is 'ancient literary schema' can feel like having the ground of their belief system reclassified out from under them, without their consent, by people they don't know and can't easily engage with as equals.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, literalist_lay_congregants, payer,
    powerless, biographical, identity_locked, local).

% Organizations built around defending a historical-scientific reading of Genesis are not treated as serious interlocutors within the academic guild that produces and enforces the literary-framework reading; their objections are addressed, if at all, as apologetics rather than scholarship, and they have no seat in the journals or seminaries that set the terms of the debate.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, young_earth_scientific_apologetics_organizations, excluded,
    organized, generational, trapped, national).

% Study the ANE textual parallels and the sociology of the reading-conflict itself without a personal stake in which reading wins within any given faith community; they document how the same kernel text is put to different institutional uses across traditions.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__literary_framework, diffuse).
narrative_ontology:fixing_cost_class(genesis_creation_cosmology__literary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a way to hold the text as authoritative scripture while accepting the findings of historical-critical scholarship and modern cosmology, avoiding a forced choice between abandoning the text or rejecting settled science.
% TRANSFER_FUNCTION: Moves interpretive authority over Genesis 1-2 away from denominational literalist bodies and toward the academic guild of biblical scholars and comparative-religion specialists; moves doctrinal certainty away from lay congregants who relied on a historical reading toward institutions that can absorb ambiguity.
% ABSENT_VOICES: Young-earth apologetics organizations and their lay constituencies would object that the reading strips the text of the truth-claims their faith communities were built around, but they are not treated as legitimate participants in the academic conversation that produces and legitimizes this reading — their objections circulate in separate publication ecosystems that rarely intersect with the mainstream guild.
% DISAPPEARANCE_RATIONALE: If this reading vanished from academic and mainline theological discourse, young-earth institutions would lose a rival framework to define themselves against but would not necessarily grow; mainline seminaries would need to find another accommodation strategy or face renewed science-scripture conflict internally. Whether the world 'rearranges' depends on whose institutional position you occupy: academic biblical studies would be substantially disrupted (a dominant interpretive paradigm removed), while for a literalist congregation nothing structural would change since the reading was never operative there.
% FOUNDING_PROBLEM: Historical-critical scholarship and comparative ANE textual studies (from the 19th century onward, accelerating with 20th-century Mesopotamian archaeology) revealed close parallels between Genesis 1-2 and other ANE creation texts, while modern cosmology and geology made a literal six-day, young-earth reading scientifically untenable for many educated readers. The framework was built to preserve the text's theological authority for audiences who could no longer hold it as a literal cosmological or historical record.
% FOUNDING_PROBLEM_CORROBORATION: Historians of biblical scholarship and secular comparative-religion academics outside any faith tradition corroborate that the ANE-parallel textual evidence is real and that the accommodation problem it responds to (science-literacy congregants unable to hold literal readings) persists; this corroboration comes from scholars with no institutional stake in whether mainline Christianity survives the accommodation, which distinguishes it from the self-interested attestations of the seminaries that adopted the reading.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__literary_framework, contested).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__literary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__literary_framework, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_cosmology__literary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__literary_framework, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__literary_framework_tests).
:- end_tests(genesis_creation_cosmology__literary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28) because the reading itself imposes no material costs — no one pays money or forfeits resources — but it does impose real costs on institutional legitimacy and personal identity coherence for those whose authority or faith structure depended on the rival reading. Suppression is low-moderate (0.22): the academic guild does not coerce anyone into adopting this reading; its force is persuasive and institutional (peer review, seminary curricula, credentialing) rather than punitive. Theater ratio is low (0.15) because the coordination function — resolving science-scripture tension for an audience that needs it resolved — is genuinely served, not merely performed. Accessibility collapse is moderate (0.35): the literalist reading remains fully available and practiced by large populations; this reading has not foreclosed it, only displaced it within certain institutional contexts. Resistance is substantial (0.55) because young-earth institutions actively contest the reading in their own literature, apologetics organizations, and denominational statements.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic biblical scholars sit at the beneficiary end: the reading is their disciplinary product, enhances their credibility, costs them nothing structurally. Young-earth authorities sit near the full-target end: their institutional identity and decades of doctrinal investment are directly devalued by the reading's ascendance in mainstream scholarship, and they cannot easily exit the conflict without abandoning what constitutes their organizations. Literalist lay congregants are targets with the least power and the most identity-fused exit constraint — an override is warranted here (see directionality_overrides) because the derived directionality from powerless+identity_locked alone would not fully capture how existentially costly the reframing is for someone whose faith formation assumed historicity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural authority with ANE comparative philology and modern cosmology) is live, not dead — comparative Semitic philology continues to develop and the science-literacy gap it addresses persists. This distinguishes the reading from mandatrophy: it is not a persisting structure whose original function has evaporated while machinery grinds on. But the disappearance_verdict is marked contested rather than clean because whether removing the reading 'rearranges the world' depends entirely on which institutional seat you ask from — a genealogical answer that itself illustrates why the kernel is contested rather than settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_identification_certainty,
    'Is the identification of Genesis 1-2 as employing ANE cosmological schema purely as literary/rhetorical device (rather than as a genuine, if culturally-bounded, cosmological claim the ancient author believed) itself a settled philological finding, or a contestable interpretive judgment shaped by the modern need to avoid conflict with contemporary science?',
    'Comparative analysis of how confident ANE literature scholars are about authorial intent versus literary function in structurally similar texts (Enuma Elish, Atrahasis) where no modern conflict-avoidance motive exists, to see whether the same deflationary reading is applied symmetrically or asymmetrically to biblical versus non-biblical ANE texts.',
    'If the deflationary genre reading is applied to Genesis specifically because it needs defending against modern science, but not applied with the same rigor to comparable ANE cosmogonies (where scholars are comfortable saying the ancient author DID believe in the cosmology described), the reading would show signs of being motivated reasoning rather than neutral philology, strengthening the young-earth camp''s charge that this is an ad hoc rescue rather than discovered textual meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_identification_certainty, conceptual, 'Whether the literary-framework genre judgment is symmetrically applied philology or asymmetric conflict-avoidance.').

omega_variable(
    kernel_reading_selection_pressure,
    'Of the three declared readings of this kernel (literary_framework, theistic_evolution, young_earth_literal), what social and institutional pressures determine which reading a given community adopts, independent of the textual evidence itself?',
    'Sociological study correlating denominational reading-adoption with variables like average congregant education level, urban/rural location, and exposure to STEM careers, to separate textual-evidentiary drivers from social-conformity drivers.',
    'If reading adoption correlates more strongly with social/educational variables than with exposure to the comparative philological evidence itself, it would suggest the kernel functions less as a text people interpret and more as a boundary marker communities use to signal in-group identity — which would reframe all three readings (including this one) as partly identity-coordination mechanisms rather than purely interpretive conclusions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether reading-selection across the kernel tracks textual evidence or social/institutional identity.').

omega_variable(
    displaced_authority_vacuum,
    'When this reading displaces both traditional theological authority (young-earth literalism) and softens theistic evolution''s residual cosmological referentiality, what fills the resulting authority vacuum for communities that adopt it — historical-critical scholarship itself, denominational leadership reinterpreting doctrine, or something more diffuse and unstable?',
    'Track doctrinal statement revisions and clergy authority structures in denominations that have formally adopted literary-framework readings over multi-decade periods, checking whether a stable replacement authority structure emerges or whether doctrinal ambiguity persists indefinitely.',
    'If no stable replacement authority emerges, the reading may function as a slow-acting solvent on denominational coherence rather than a genuine resolution, which would revise the coordination_function claim upward in extractiveness over a longer time horizon than this story''s interval captures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_authority_vacuum, conceptual, 'What fills the interpretive-authority vacuum this reading creates once adopted institutionally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__literary_framework, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_cosmology__literary_framework, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_cosmology__literary_framework, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_cosmology__literary_framework, theater_ratio, 40, 0.12).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_cosmology__literary_framework, theater_ratio, 60, 0.13).
narrative_ontology:measurement(gene_tr_t80, genesis_creation_cosmology__literary_framework, theater_ratio, 80, 0.14).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_cosmology__literary_framework, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_cosmology__literary_framework, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gene_be_t20, genesis_creation_cosmology__literary_framework, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(gene_be_t40, genesis_creation_cosmology__literary_framework, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(gene_be_t60, genesis_creation_cosmology__literary_framework, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(gene_be_t80, genesis_creation_cosmology__literary_framework, base_extractiveness, 80, 0.27).
narrative_ontology:measurement(gene_be_t100, genesis_creation_cosmology__literary_framework, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(genesis_creation_cosmology__literary_framework, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__literary_framework, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__literary_framework, young_earth_literal).
narrative_ontology:affects_constraint(genesis_creation_cosmology__literary_framework, theistic_evolution).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the genesis_creation_cosmology kernel. literary_framework (this story) is the most deflationary reading, denying cosmological reference entirely. theistic_evolution retains cosmological reference but treats it as figurative/theological rather than literal-historical. young_earth_literal retains full literal-historical cosmological reference. All three share the same kernel text (Genesis 1-2) but instantiate structurally distinct constraints with distinct beneficiary/victim sets, distinct ε profiles, and distinct authority-displacement patterns. Link all three via affects_constraints; each carries its own base_properties independently per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genesis_creation_cosmology__literary_framework, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
