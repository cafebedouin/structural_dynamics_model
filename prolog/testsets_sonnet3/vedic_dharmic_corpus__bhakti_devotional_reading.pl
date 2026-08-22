% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading of the Vedic-Dharmic Corpus
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the bhakti devotional reading of the contested
 *   Vedic-dharmic corpus kernel: the claim that sincere devotion (bhakti),
 *   not birth into a lineage, determines access to the divine and to
 *   spiritual authority. Historically expressed through poet-saints such as
 *   Kabir, Ravidas, Mirabai, the Alvars, and the Nayanars, and
 *   institutionalized in sampradayas from the medieval period onward, this
 *   reading opened religious participation and teaching authority to persons
 *   regardless of caste. It coordinates religious life around a genuinely
 *   lower-overhead standard (demonstrated devotion) than the
 *   hereditary-lineage alternative, but does not fully dissolve caste
 *   practice — some temple institutions and ritual customs retain caste- and
 *   gender-based exclusions that the theology itself does not require but
 *   also does not eliminate. This is why the victim set shrinks relative to a
 *   hereditary-monopoly reading but does not vanish, and why extraction is
 *   authored at a moderate ~0.40 rather than near zero.
 *
 * KEY AGENTS:
 *   - bhakti_sant_lineages: primary agenda-setters who articulate and institutionalize the devotional reading (organized/mobile)
 *   - devotee_communities and non_brahmin_devotees: primary beneficiaries gaining religious standing without lineage credential (moderate/mobile)
 *   - dalit_devotees_facing_residual_temple_exclusion and women_devotees_facing_residual_ritual_restriction: bear the residual cost where doctrine and practice diverge (powerless/constrained)
 *   - brahmin_ritual_specialists: excluded from doctrinal necessity though sometimes absorbed as devotional gurus (organized/constrained)
 *   - constitutional_and_academic_observers: analytical seat assessing actual historical effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading of the Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious_authority/social_stratification/interpretive_legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '22d805de-80f0-4f2a-8a51-3fc29326296c').
narrative_ontology:cs_kernel_codification('22d805de-80f0-4f2a-8a51-3fc29326296c', distributed).
narrative_ontology:cs_authority_grounding('22d805de-80f0-4f2a-8a51-3fc29326296c', practice).
narrative_ontology:cs_interpretation_layer_present('22d805de-80f0-4f2a-8a51-3fc29326296c').
narrative_ontology:cs_reading_relation('22d805de-80f0-4f2a-8a51-3fc29326296c', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('22d805de-80f0-4f2a-8a51-3fc29326296c', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('22d805de-80f0-4f2a-8a51-3fc29326296c', foundational, devotion_not_birth_grounds_spiritual_authority).
narrative_ontology:cs_axiom_status(devotion_not_birth_grounds_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('22d805de-80f0-4f2a-8a51-3fc29326296c', devotion_not_birth_grounds_spiritual_authority, deontological).
narrative_ontology:cs_axiom('22d805de-80f0-4f2a-8a51-3fc29326296c', secondary, direct_experiential_access_to_divine_requires_no_intermediary).
narrative_ontology:cs_axiom_status(direct_experiential_access_to_divine_requires_no_intermediary, holdable).
narrative_ontology:cs_axiom_grounding('22d805de-80f0-4f2a-8a51-3fc29326296c', direct_experiential_access_to_divine_requires_no_intermediary, conventional).
narrative_ontology:cs_reference_frame('22d805de-80f0-4f2a-8a51-3fc29326296c', pre_bhakti_brahmanical_ritual_exclusivity).
narrative_ontology:cs_drift_state('22d805de-80f0-4f2a-8a51-3fc29326296c', contemporary_devotional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('22d805de-80f0-4f2a-8a51-3fc29326296c', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotee_communities).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_sant_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, dalit_devotees_facing_residual_temple_exclusion).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees_facing_residual_ritual_restriction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, brahmin_ritual_specialists).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotion_confers_spiritual_authority).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, divine_access_is_not_birth_conditioned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Poet-saints and their successor lineages (Kabir, Ravidas, Mirabai, Alvars, Nayanars, Chaitanya-tradition gurus and their institutional descendants) articulate and transmit the devotional reading, holding that direct emotional and ritual surrender to the divine grants spiritual standing independent of birth. They administer temples, sampradayas, and devotional societies organized around this premise, and derive their own religious authority and following from having authored or inherited the interpretive move that opened divine access beyond caste lines.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_sant_lineages, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_sant_lineages, beneficiary).

% Lay practitioners across caste backgrounds who participate in bhakti worship — kirtan, satsang, pilgrimage, devotional societies — gaining a route to religious standing, community belonging, and ritual participation that does not require Brahmin intermediation or genealogical credential. They can exit into other devotional lineages or secular life relatively freely; the coordination benefit is real and largely uncoerced.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotee_communities, beneficiary,
    moderate, biographical, mobile, regional).

% Devotees from shudra, artisan, and other non-Brahmin backgrounds who take up positions as bhakti poets, temple functionaries, or spiritual teachers within devotional lineages that recognize devotion over lineage. Historical examples (Ravidas, Tukaram, Kabir) show mobility into recognized spiritual authority that hereditary readings would have foreclosed; contemporary devotees gain analogous, if uneven, standing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_devotees, beneficiary,
    moderate, biographical, mobile, regional).

% Devotees from historically untouchable communities who embrace bhakti's theological claim of caste-blind divine access but still encounter denial of temple entry, separate worship spaces, or exclusion from priestly roles at particular shrines where local custom or brahminical temple management persists alongside devotional theology. The devotional reading grants theological standing without guaranteeing corresponding institutional access, leaving a residual gap between doctrine and practice that they bear the cost of.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, dalit_devotees_facing_residual_temple_exclusion, payer,
    powerless, biographical, constrained, local).

% Women devotees whom bhakti theology also addresses as spiritually capable through devotion (Mirabai, Andal, Akka Mahadevi are canonical examples) but who continue to encounter menstruation-based exclusion, restricted access to certain shrines, or exclusion from ordained teaching roles within some devotional institutions, despite the doctrine's own logic implying no such bar. They absorb the cost of doctrine-practice inconsistency that the devotional reading itself does not fully resolve.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees_facing_residual_ritual_restriction, payer,
    powerless, biographical, constrained, local).

% Brahmin priests whose ritual monopoly is structurally bypassed by the devotional reading's premise that sincere devotion alone confers access to the divine. Some Brahmin bhakti figures (in the Chaitanya and Vallabha traditions, for instance) are absorbed into devotional lineages as gurus, but the reading as a doctrine does not require their mediation, and its wider adoption reduces the necessity of purely hereditary priestly service. Their objection — that ritual competence and lineage transmission carry irreplaceable technical content — is heard within some sampradayas but does not govern the devotional reading's own logic.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, brahmin_ritual_specialists, excluded,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, brahmin_ritual_specialists, beneficiary).

% Historians of religion, legal scholars, and constitutional commentators who study the bhakti movement's actual historical effects on caste practice — documenting where devotional theology produced genuine institutional opening (temple entry movements, non-Brahmin priesthoods) and where it coexisted with unchanged social stratification. They take testimony from devotee and excluded-caste seats without themselves administering the tradition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, constitutional_and_academic_observers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared devotional practice (bhakti) through which persons of any caste background can pursue and be recognized as having attained spiritual standing, solving the coordination problem of religious participation and status recognition without requiring a birth-credentialing apparatus for every worshipper.
% TRANSFER_FUNCTION: Moves recognized spiritual authority and community standing away from exclusive Brahmin-lineage custody and toward whoever demonstrates sincere devotional practice; moves some ritual-service demand away from hereditary priests toward bhakti teachers and lineage gurus, without a corresponding monetary transfer of the kind seen in extractive constraints.
% ABSENT_VOICES: Local temple management committees and orthodox ritual authorities who maintain caste-based entry practices at specific sites are not parties to the doctrinal debate; their continued exclusionary practice persists below the level the devotional reading's theology addresses, and they are rarely named directly when the doctrine is praised in the abstract.
% DISAPPEARANCE_RATIONALE: If the devotional reading vanished as a live theological option, the historical and institutional space occupied by bhakti sampradayas, non-Brahmin spiritual teachers, and caste-inclusive devotional societies would lose its doctrinal justification; hereditary ritual authority would regain uncontested textual grounding, and millions of devotees' claimed standing to worship and teach without lineage credential would lose its theological basis, forcing renegotiation of temple access, teaching authority, and community leadership along purely hereditary lines.
% FOUNDING_PROBLEM: Historically, exclusive Brahmin-hereditary control of ritual and interpretive access to the divine excluded the great majority of the population — shudras, so-called untouchable communities, and women — from direct religious standing; the bhakti movements (roughly 6th century CE onward, intensifying through the medieval period) arose to establish that devotion, not birth, opens access to the divine and to religious authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the bhakti movement and religious studies scholars outside any single sampradaya attest that the movement produced real, if geographically and institutionally uneven, expansion of religious participation across caste lines (temple-entry movements, non-Brahmin poet-saints achieving canonical status). Dalit and women devotee testimony and contemporary anti-caste scholarship corroborate that the founding problem is only partially resolved: the doctrine's caste-blind theology coexists with persistent caste- and gender-based practice at many sites, which is why the status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.40 because the devotional reading, while genuinely coordination-oriented and largely voluntary in its uptake, does not itself dismantle every caste- and gender-based institutional practice that persists alongside it — the residual gap between theological claim and lived temple/ritual practice is where the moderate extraction sits, borne by the two payer groups. Suppression is comparatively low (0.35) because adoption of bhakti practice is rarely coerced and exit into other devotional or secular paths is generally available; what suppression exists is localized to specific institutions defending residual exclusionary custom, not to the devotional reading's own logic. Theater ratio is low-to-moderate and rises slowly over the interval as some institutionalized bhakti organizations formalize their own hierarchies (guru lineages, sampradaya governance) that layer some performative structure atop originally anti-hierarchical practice.
 *
 * PERSPECTIVAL GAP:
 *   From the bhakti sant lineages' and devotee communities' seats, this reading functions as coordination — a genuine widening of access to religious standing. From the seat of Dalit and women devotees who still encounter residual exclusion at particular sites, the same corpus-reading is experienced as an unfulfilled promise: the theology says one thing, the institution sometimes does another. The engine should register this as seat divergence within a fundamentally rope-classified reading rather than as evidence the reading itself is a snare — the residual extraction is real but bounded, not the reading's central mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Bhakti sant lineages and devotee/non-Brahmin communities sit near the beneficiary end: they gain standing, community, and (for lineage leaders) institutional authority through the devotional framework, and their exit options are generally mobile. Dalit and women devotees sit toward the target end because, notwithstanding the theology's own inclusive logic, they carry the residual cost of unreformed local practice with constrained exit (leaving a devotional community does not necessarily open temple doors elsewhere). Brahmin ritual specialists are excluded from doctrinal centrality but retain organized power and often adapt by joining devotional institutions as gurus, which is why their exit option is authored as constrained rather than trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The devotional reading is not mandatrophic in the way a hereditary-monopoly reading risks becoming (a founding problem long dead but the arrangement persisting for its own sake) — the founding problem (caste-based exclusion from direct devotion) is authored as contested rather than dead precisely because residual exclusionary practice persists at specific institutions even where the theology has moved on. This prevents two errors: mislabeling the reading as pure extraction (ignoring its real coordination and historically documented mobility effects) and mislabeling it as fully resolved coordination (ignoring the corroborated residual victim set).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_kernel_reading_ambiguity,
    'Is the bhakti devotional reading a genuine alternative interpretive tradition within the same textual kernel as the hereditary-monopoly reading, or is it better understood as a distinct historical corpus (bhakti literature, vernacular devotional poetry) only loosely continuous with the Vedic-Brahmanical textual core the hereditary reading claims?',
    'Textual-historical analysis of citation and self-positioning: do bhakti poet-saints explicitly claim continuity with or supersession of Vedic authority, or do they largely operate outside its citation structure, appealing instead to direct experience?',
    'If bhakti is a substantially independent tradition rather than a reading of the same kernel, the three-reading kernel model should be revised to either add bhakti as its own quasi-kernel or narrow the shared-kernel claim to specific textual overlaps (Bhagavad Gita, Puranic material) rather than the full Vedic-dharmic corpus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_kernel_reading_ambiguity, conceptual, 'Whether bhakti devotionalism is a reading of the same kernel as hereditary Vedic authority or a structurally separate tradition.').

omega_variable(
    doctrine_practice_gap_measurement,
    'How large, in practice, is the residual gap between bhakti''s caste-blind theological claim and actual institutional access at temples and devotional organizations across regions and time periods?',
    'Comparative institutional survey of temple-entry policy, priestly appointment records, and devotee testimony across bhakti-influenced institutions versus hereditary-Brahmin-controlled institutions, sampled across regions and centuries.',
    'A large, persistent gap would push extractiveness upward toward tangled_rope territory (real coordination function coexisting with substantial unaddressed extraction); a small, closing gap would support classifying the reading closer to a pure rope with negligible extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_gap_measurement, empirical, 'The empirical magnitude of doctrine-practice divergence in bhakti institutions.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the devotional reading''s core premise (devotion, not birth, determines spiritual authority) logically foreclose the hereditary-monopoly reading''s premise, or can both be held simultaneously by different institutional actors within the same broader tradition (as historically occurred, with some Brahmin lineages absorbing bhakti practice without renouncing hereditary ritual privilege)?',
    'Examine whether any historical or contemporary institution has coherently held both premises without internal contradiction (e.g., a temple that admits devotional access to some spiritual roles while reserving specific ritual functions for hereditary priests).',
    'If such hybrid institutions are coherent and common, the relation to hereditary_monopoly_reading should be coexists_with (as authored); if hybrid holding is shown to be unstable or self-contradictory in practice, the relation should be reconsidered toward forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the devotional and hereditary readings can coexist within single institutions or are logically incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_dharmic_corpus__bhakti_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vedic_dharmic_corpus kernel. hereditary_monopoly_reading authors the same textual inheritance as grounding a birth-determined ritual monopoly (high extraction, clear beneficiary class, snare-adjacent); reformist_egalitarian_reading authors it as requiring conformity to constitutional equality with caste read as historical accretion (contested authority grounding, potentially scaffold-like transitional framing). bhakti_devotional_reading (this story) authors moderate extraction, diffuse beneficiary structure, and a genuine but incomplete coordination function. All three share the kernel but are authored with independent ε values per the ε-invariance principle; none averages or references the others' classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
