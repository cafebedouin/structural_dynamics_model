% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary Continuity Definition of Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The literary continuity reading defines a language as 'living' if it
 *   sustains new literary and intellectual production, irrespective of native
 *   speaker transmission. The Haskalah (Jewish Enlightenment) Hebrew press
 *   (1780s onward) and subsequent modern Hebrew literature (Mendele, Bialik,
 *   Agnon) are cited as proof that Hebrew remained vital through textual
 *   creativity alone. This reading was developed by maskilim to claim
 *   cultural authority for their secular Hebrew project without requiring
 *   mass vernacular adoption — a coordination function for a dispersed
 *   intelligentsia. It simultaneously excluded Yiddish-speaking masses and
 *   traditional religious elites from the 'vitality' franchise, transferring
 *   legitimacy to the new literary elite. The constraint persists in Israeli
 *   national discourse and sociolinguistic theory (e.g., 'revival' vs.
 *   'continuity' debates).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.35).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.42).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary Continuity Definition of Living Language Status").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '1fb60f21-5b31-4507-baf8-26866118ea2b').
narrative_ontology:cs_kernel_codification('1fb60f21-5b31-4507-baf8-26866118ea2b', distributed).
narrative_ontology:cs_authority_grounding('1fb60f21-5b31-4507-baf8-26866118ea2b', practice).
narrative_ontology:cs_interpretation_layer_present('1fb60f21-5b31-4507-baf8-26866118ea2b').
narrative_ontology:cs_reading_relation('1fb60f21-5b31-4507-baf8-26866118ea2b', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fb60f21-5b31-4507-baf8-26866118ea2b', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('1fb60f21-5b31-4507-baf8-26866118ea2b', foundational, literary_productivity_suffices_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('1fb60f21-5b31-4507-baf8-26866118ea2b', literary_productivity_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('1fb60f21-5b31-4507-baf8-26866118ea2b', secondary, native_speaker_status_not_necessary).
narrative_ontology:cs_axiom_status(native_speaker_status_not_necessary, holdable).
narrative_ontology:cs_axiom_grounding('1fb60f21-5b31-4507-baf8-26866118ea2b', native_speaker_status_not_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('1fb60f21-5b31-4507-baf8-26866118ea2b', maskilic_literary_vitality).
narrative_ontology:cs_drift_state('1fb60f21-5b31-4507-baf8-26866118ea2b', modern_hebrew_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1fb60f21-5b31-4507-baf8-26866118ea2b', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, zionist_revivalists).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_productivity_suffices_for_vitality).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, hebrew_as_living_language_pre_revival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish Enlightenment intellectuals who produced Hebrew periodicals (Ha-Me'assef, Ha-Melitz, etc.) and defined Hebrew vitality through literary output. They gained cultural authority and institutional positions (schools, press, societies) without requiring mass vernacular adoption. Their exit options included integration into Russian/European intellectual circles or Zionist institution-building.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim, beneficiary).

% Writers, critics, and scholars (e.g., Mendele Moykher-Sforim, Bialik, Ahad Ha'am) who built modern Hebrew literature. They benefited from the literary-vitality definition because it validated their work as nation-building without demanding spoken fluency from the masses. Could exit to Yiddish, Russian, or German literary fields.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_intellectuals, beneficiary,
    organized, biographical, mobile, regional).

% Yiddish-speaking Jews (mostly women, rural poor, traditional cheder-educated men) who used Hebrew only liturgically. The literary-vitality definition excluded their lived language practices from 'vitality,' rendering their linguistic world invisible or 'dead.' No exit from this exclusion — the definition itself marks them as non-participants in the living language.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_speakers, payer,
    powerless, immediate, trapped, local).

% Speakers of Jewish vernaculars (Yiddish, Judeo-Arabic, Ladino) who conducted daily life, commerce, and oral culture in those languages. The literary Hebrew definition positioned their languages as 'corpse' or 'jargon,' denying them vitality status. Could shift to Hebrew literary culture only through education they often lacked.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, non_literary_speakers, payer,
    moderate, biographical, constrained, regional).

% Rabbinic leadership who defined Hebrew vitality through liturgical study and sacred text transmission (the liturgical_preservation_reading). They were excluded from the maskilic literary public sphere and would object to secular literature as the vitality metric. Their exit was constrained by communal authority structures.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, traditional_religious_authorities, excluded,
    institutional, generational, constrained, regional).

% Political activists (Ben-Yehuda, Second Aliyah pioneers) who adopted the literary continuity claim to legitimize spoken Hebrew revival. They benefited from the pre-existing literary corpus as proof of viability. Could pivot to other nationalist languages but invested heavily in Hebrew.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, zionist_revivalists, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, zionist_revivalists, agenda_setter).

% Sociolinguists and historians (e.g., Joshua Fishman, Benjamin Harshav) who analyze vitality definitions as analytical objects. They see the full structural field — the literary continuity reading as one contested framework among others — and are not subject to its exclusions.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguistic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates elite cultural production around a shared literary language, enabling intellectual exchange, canon formation, and nationalist symbolization across a dispersed diaspora without requiring spoken vernacular unity.
% TRANSFER_FUNCTION: Moves cultural authority, institutional recognition, and nationalist legitimacy from traditional religious elites to secular literary intellectuals; moves the 'vitality' label from spoken vernaculars (Yiddish, etc.) to the literary Hebrew corpus.
% ABSENT_VOICES: Illiterate and non-literary speakers (mostly women, rural poor, traditional Jews) who would object to their linguistic practices being classified as 'dead' or 'non-vital.' They were structurally excluded from the Hebrew press, maskilic societies, and later Zionist institutions where the definition was negotiated.
% DISAPPEARANCE_RATIONALE: If the literary-continuity definition vanished, the symbolic foundation for Hebrew revival as 'continuation not invention' would collapse; Yiddish and other vernaculars would gain retrospective vitality recognition; secular intellectual authority would lose its distinct legitimation channel; Israeli national culture would lose its primary pre-state legitimacy anchor.
% FOUNDING_PROBLEM: How to claim Hebrew as a living national language for a people who had not spoken it natively for 1,700 years, without conceding that Yiddish or other vernaculars were the 'real' living languages of the Jews.
% FOUNDING_PROBLEM_CORROBORATION: Maskilim and Zionist historians (e.g., Klausner, Rawidowicz) attest the problem was real and the literary definition solved it. Yiddishists (e.g., Zhitlowsky, Weinreich) and sociolinguists (Fishman) attest the problem was artificially constructed to marginalize vernaculars — the 'dead language' premise was a political claim, not a linguistic fact. No consensus outside the benefiting intellectual lineages.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.35) because the constraint primarily coordinates elite literary production rather than extracting material resources. Suppression (0.42) is moderate — the definition's persistence requires active maintenance through academic canonization, school curricula, and institutional exclusion of competing vitality definitions (Yiddishist, liturgical). Theater ratio is low (0.18) — the literary production is genuine, not performative. Accessibility collapse (0.48) reflects that alternatives (Yiddish vitality, liturgical vitality) remain conceptually available but are structurally marginalized. Resistance (0.52) is significant from both traditional religious and Yiddishist camps. The measurement series tracks the constraint's consolidation from early Haskalah (low extraction, low suppression) to post-WWI Zionist institutionalization (higher extraction via state-building, higher suppression via educational monopoly).
 *
 * PERSPECTIVAL GAP:
 *   From the maskil/secular intellectual seat, the constraint is a Rope — genuine coordination enabling a dispersed people to maintain cultural unity through literature. From the illiterate/non-literary speaker seat, it is a Snare — their lived languages are declared dead to elevate a corpus they cannot access. From the traditional authority seat, it is a Tangled Rope — liturgical coordination is real but the secular literary layer extracts authority. The engine computes this divergence; the claimed_type (tangled_rope) reflects the structural hybridity visible from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals are structural beneficiaries (d ~0.15): they gain cultural authority, publication venues, and nationalist legitimacy from the definition. Illiterate and non-literary speakers are structural targets (d ~0.85): the definition extracts their linguistic reality by classifying it as 'non-vital,' with trapped/constrained exit. Traditional religious authorities are excluded (d ~0.7): their vitality definition is suppressed but they retain institutional power. Zionist revivalists are secondary beneficiaries (d ~0.2): they inherit the literary corpus as legitimacy capital. Linguistic scholars are analytical observers (d ~0.5). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing Hebrew as a living national language without native speakers) was live in 1780-1920. By 1920, spoken Hebrew revival in Palestine made the literary-continuity claim partially obsolete — native speakers now existed. Yet the definition persists because it still serves: (1) legitimating diaspora Hebrew culture as 'living' without aliyah, (2) anchoring Israeli national identity in a 3,000-year textual chain, (3) marginalizing Yiddish and Arabic-Jewish heritage as 'dead.' The mandate has atrophied for its original coordination function but persists for extraction of symbolic capital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'How does this reading''s structural profile (beneficiaries, victims, ε) differ from its sibling readings of the same kernel?',
    'Author the sibling readings (liturgical_preservation_reading, native_generation_reading) as separate constraint stories with their own ε, beneficiaries, victims, and claimed_type. Compare the three ε values and beneficiary/victim structures to map the kernel''s contested topology.',
    'If sibling readings show substantially different ε values (e.g., native_generation_reading has high ε for Yiddish speakers), the kernel decomposes into structurally distinct constraints per ε-invariance. If ε values are similar, the contest is interpretive not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel; structural differences across readings must be mapped via separate stories.').

omega_variable(
    literary_vitality_naturalness,
    'Is the literary-continuity definition of language vitality a discovered linguistic fact or a constructed ideological claim?',
    'Compare vitality assessments across language communities: do sociolinguists outside Jewish studies accept literary productivity as sufficient for vitality (e.g., Latin, Sanskrit, Classical Arabic)? If yes, the definition has cross-linguistic traction; if no, it is a local ideological construction.',
    'If constructed, the constraint is a Tangled Rope or Snare with identifiable beneficiaries. If a genuine linguistic natural law, it trends toward Mountain (but FSM would trigger due to beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_vitality_naturalness, conceptual, 'Natural-law vs. constructed ambiguity for a Mountain-claimed constraint — but this story claims Tangled Rope, so the omega documents the classification uncertainty.').

omega_variable(
    hebrew_exceptionalism_evidence,
    'Does the Hebrew case genuinely demonstrate literary continuity as vitality, or is it a unique revival retrofitted with a continuity narrative?',
    'Historical analysis: was there an unbroken chain of *productive* literary work (new genres, original thought) from Haskalah to revival, or did the revival create a new spoken language that appropriated the literary corpus? Assess the ''productivity'' metric independently of nationalist historiography.',
    'If the literary continuity is substantially invented, the constraint''s coordination function is mythological — extraction of symbolic capital from a fabricated genealogy. If genuine, the coordination function is real and ε remains low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hebrew_exceptionalism_evidence, empirical, 'Whether the flagship evidence (Haskalah-to-modern-Hebrew continuity) supports the reading''s coordination claim or is a retrospective construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1780, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llslc_tr_t1780, living_language_status__literary_continuity_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement_basis(llslc_tr_t1780, observed).
narrative_ontology:measurement(llslc_tr_t1820, living_language_status__literary_continuity_reading, theater_ratio, 1820, 0.08).
narrative_ontology:measurement_basis(llslc_tr_t1820, observed).
narrative_ontology:measurement(llslc_tr_t1860, living_language_status__literary_continuity_reading, theater_ratio, 1860, 0.12).
narrative_ontology:measurement_basis(llslc_tr_t1860, observed).
narrative_ontology:measurement(llslc_tr_t1880, living_language_status__literary_continuity_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement_basis(llslc_tr_t1880, observed).
narrative_ontology:measurement(llslc_tr_t1900, living_language_status__literary_continuity_reading, theater_ratio, 1900, 0.17).
narrative_ontology:measurement_basis(llslc_tr_t1900, observed).
narrative_ontology:measurement(llslc_tr_t1920, living_language_status__literary_continuity_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement_basis(llslc_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(llslc_be_t1780, living_language_status__literary_continuity_reading, base_extractiveness, 1780, 0.15).
narrative_ontology:measurement_basis(llslc_be_t1780, observed).
narrative_ontology:measurement(llslc_be_t1820, living_language_status__literary_continuity_reading, base_extractiveness, 1820, 0.22).
narrative_ontology:measurement_basis(llslc_be_t1820, observed).
narrative_ontology:measurement(llslc_be_t1860, living_language_status__literary_continuity_reading, base_extractiveness, 1860, 0.28).
narrative_ontology:measurement_basis(llslc_be_t1860, observed).
narrative_ontology:measurement(llslc_be_t1880, living_language_status__literary_continuity_reading, base_extractiveness, 1880, 0.32).
narrative_ontology:measurement_basis(llslc_be_t1880, observed).
narrative_ontology:measurement(llslc_be_t1900, living_language_status__literary_continuity_reading, base_extractiveness, 1900, 0.34).
narrative_ontology:measurement_basis(llslc_be_t1900, observed).
narrative_ontology:measurement(llslc_be_t1920, living_language_status__literary_continuity_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement_basis(llslc_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(llslc_su_t1780, living_language_status__literary_continuity_reading, suppression_requirement, 1780, 0.25).
narrative_ontology:measurement_basis(llslc_su_t1780, observed).
narrative_ontology:measurement(llslc_su_t1820, living_language_status__literary_continuity_reading, suppression_requirement, 1820, 0.3).
narrative_ontology:measurement_basis(llslc_su_t1820, observed).
narrative_ontology:measurement(llslc_su_t1860, living_language_status__literary_continuity_reading, suppression_requirement, 1860, 0.35).
narrative_ontology:measurement_basis(llslc_su_t1860, observed).
narrative_ontology:measurement(llslc_su_t1880, living_language_status__literary_continuity_reading, suppression_requirement, 1880, 0.38).
narrative_ontology:measurement_basis(llslc_su_t1880, observed).
narrative_ontology:measurement(llslc_su_t1900, living_language_status__literary_continuity_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement_basis(llslc_su_t1900, observed).
narrative_ontology:measurement(llslc_su_t1920, living_language_status__literary_continuity_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement_basis(llslc_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.08).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the living_language_status kernel. The literary_continuity_reading defines vitality by literary production (beneficiaries: maskilim/secular intellectuals; victims: illiterate/non-literary speakers; ε=Low). The liturgical_preservation_reading defines vitality by sacred recitation (beneficiaries: religious authorities; victims: secularists; ε=Low). The native_generation_reading defines vitality by mother-tongue transmission (beneficiaries: Yiddishists/vernacular speakers; victims: Hebrew revivalists; ε=Moderate). The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
