% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Continuity of Classical Latin
 *   domain: intellectual_history/linguistics
 *
 * SUMMARY:
 *   The continuity reading of Latin correctness asserts that Medieval Latin
 *   represents the legitimate evolution of Classical Latin through natural
 *   linguistic processes — phonological simplification, grammatical
 *   restructuring, vocabulary expansion — rather than corruption or rupture.
 *   The reading treats medieval scribes and clergy as rightful inheritors of
 *   the classical tradition, participating in a continuous chain of
 *   transmission and development. Under this reading, the constraint operates
 *   with minimal extractiveness because no party claims monopoly on what
 *   counts as legitimate Latin; instead, the reading authorizes multiple
 *   registers and contexts (classical for copying ancient authorities,
 *   medieval for practical communication) as coexisting legitimate forms. The
 *   constraint persists because it solves a genuine coordination problem —
 *   maintaining Latin literacy after the death of native speakers — not
 *   because it concentrates benefits on a victimizing beneficiary.
 *
 * KEY AGENTS:
 *   - Medieval scribes and clergy: moderate power, continental scope. Inherit and transmit the classical corpus while adapting Latin to living speech patterns. Benefit from the continuity reading's authorization of medieval forms as legitimate evolution.
 *   - Monastic intellectual community: institutional power, continental scope, generational time horizon. Agenda-setter role — defines the interpretive frame that legitimates medieval developments. Constrains new transmission through educational training and copying practices.
 *   - Vernacular speakers becoming literate: powerless, constrained exit, regional scope. Become literate in Latin through forms closer to their native speech. The continuity reading permits their education without treating them as degrading the language.
 *   - Classical purist grammarians: excluded from medieval intellectual authority. Would argue for a fixed standard. Represent the alternative rupture reading but lack institutional power during the medieval period to enforce it.
 *   - Humanist Renaissance scholars: future observers who will radically contest and displace the continuity reading by excavating classical Latin as a recovered, purified standard.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.15).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.22).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Legitimate Continuity of Classical Latin").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "intellectual_history/linguistics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '6c7919bb-6e8d-49c2-a8f1-3347ea074c72').
narrative_ontology:cs_kernel_codification('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', fixed_text).
narrative_ontology:cs_authority_grounding('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', lineage).
narrative_ontology:cs_interpretation_layer_present('6c7919bb-6e8d-49c2-a8f1-3347ea074c72').
narrative_ontology:cs_reading_relation('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', foundational, linguistic_change_as_natural_evolution).
narrative_ontology:cs_axiom_status(linguistic_change_as_natural_evolution, holdable).
narrative_ontology:cs_axiom_grounding('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', linguistic_change_as_natural_evolution, empirically_contingent).
narrative_ontology:cs_axiom('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', foundational, transmission_authority_over_textual_authority).
narrative_ontology:cs_axiom_status(transmission_authority_over_textual_authority, holdable).
narrative_ontology:cs_axiom_grounding('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', transmission_authority_over_textual_authority, conventional).
narrative_ontology:cs_reference_frame('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', living_transmission_framework).
narrative_ontology:cs_drift_state('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', end_medieval_period_1400, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6c7919bb-6e8d-49c2-a8f1-3347ea074c72', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_scribes_and_clergy).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, vernacular_speakers_becoming_literate).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, monastic_intellectual_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end) because the constraint does not concentrate rents on an identified beneficiary — medieval clergy and scribes benefit from permission to use living forms, but they do not extract this permission from anyone; the constraint simply authorizes what people are already doing. Suppression rises modestly over the interval (0.1 to 0.22) as monastic institutions formalize schooling and copyist practices — the constraint requires some active defense against purist objections and against the temptation to revert to purely classical forms, but this is not coercive suppression of alternatives, rather institutional maintenance of a scholarly consensus. Theater ratio remains low (0.05 to 0.20 across the interval) because the core function — maintaining Latin as a working language — is genuine; any performative element (copying classical texts as if they were the only legitimate form) is secondary to practical communication. Accessibility collapse is moderate (0.65) because alternatives do exist: monks could choose to speak Romance dialects exclusively, or attempt to enforce Ciceronian standards on all writing. The constraint persists because it works, not because alternatives have been suppressed beyond recovery. Resistance is low (0.35) because the constraint aligns with natural language change and the interests of the literate community — the main resistance comes from later purists, not from medieval participants themselves.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (monastic community) experiences the constraint as coordinating literacy and transmission — a genuine solution to the problem of maintaining classical learning in a Romance-speaking world. The excluded purists would experience it as legitimating error and hastening corruption. The later humanists (observed from outside the interval) experience it as a naïve embrace of medieval degradation that obscures the true, recoverable form of classical Latin. The engine measures these divergences through power, exit_options, and beneficiary/victim structure: the institutional beneficiaries compute a low-extraction type (rope/coordination), while the excluded purists (if they had power and stake) would compute a higher-extraction type. The disjuncture itself is diagnostic — it shows the continuity reading working by consensus and institutional authority, not by coercive suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval scribes and clergy are structural beneficiaries (d ≈ 0.2): they gain permission to use evolved forms without losing prestige, face low exit costs, and benefit from the monastic community's intellectual endorsement. Vernacular speakers have constrained exits (identity_locked in some cases — becoming a Latin-literate clergyman means adopting the monastic episteme) but benefit from access to literacy (d ≈ 0.3). The monastic intellectual community is the agenda-setter (d ≈ 0.15 as beneficiary, but power-atom institutional): they set the frame and constrain practice through education and copying standards. Classical purists are excluded (d ≈ 0.85): the constraint operates by denying the legitimacy of their fixed-standard interpretation. They remain alive as a minority position in the intellectual record but lack institutional power during the medieval period. Humanist observers (analytical seat, d = 0.5) will eventually displace this reading entirely by recovering classical texts and establishing the rupture reading as hegemonic. No directionality override is needed: the structural derivation from beneficiary/victim + exit produces accurate d values for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading has no mandatrophy problem: the founding problem (maintaining Latin literacy after native speakers cease to exist) remains live throughout the medieval period and beyond. The constraint's function — coordinating literary transmission and scholarly work — persists because people keep writing in Latin, copying texts, and using it for communication across Romance-speaking regions. The constraint does not persist by inertia or by defending a dead mandate; it persists because the mandate is constantly renewed by use. The theater_ratio measurements show a modest rise (0.05 to 0.20) but remain well below the piton threshold (0.50+), indicating that functional activity (actual writing, copying, teaching) remains substantially above performative activity (ritualistic adherence to classical forms for their own sake). The measurement series would show a sharp collapse of this reading in the 15th–16th centuries when humanist scholarship excavates classical Latin as a recoverable standard and the rupture reading becomes institutionally dominant — but that shift is outside the interval. Within the medieval period (500–1400), mandatrophy is absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_change_vs_corruption_framing,
    'Is linguistic change from classical to medieval Latin the result of natural, inevitable linguistic processes (sound change, grammatical reanalysis), or the result of ignorance and corruption of a fixed standard by less-educated users?',
    'Comparative study of documented language change in other documented language families with known ancestral forms (Romance languages'' evolution from Vulgar Latin, Germanic languages from Proto-Germanic, etc.). Application of Historical Linguistics principles to reconstruct the change mechanism.',
    'If the changes are demonstrated to follow predictable natural patterns (deletion of unstressed vowels, grammatical reanalysis under contact stress), the continuity reading is strengthened and extractiveness remains low. If the changes are shown to be random corruption, the rupture reading gains traction and extractiveness would rise (someone would need to enforce classical standards against natural drift).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_change_vs_corruption_framing, empirical, 'Whether medieval linguistic forms represent natural evolution or corruption of a fixed standard.').

omega_variable(
    epistemic_authority_locus,
    'Who has the authority to define correctness in Latin — living speakers and writers who continue the tradition, or the recovered classical corpus as a reference standard?',
    'Examination of how monastic scholars and later humanists justify their choices: do they appeal to living practice and transmission, or to textual authority and classical exemplars? Textual evidence from glossaries, grammar treatises, and manuscript variants showing which authority is invoked.',
    'If authority is vested in living practice and transmission, the continuity reading holds and extractiveness remains low. If authority is vested in the classical corpus, the rupture reading gains traction — someone (classical scholars, later humanists) would need to enforce conformity to the textual standard, raising extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_locus, conceptual, 'Where the authority to define linguistic correctness resides — in living continuation or in recovered textual standards.').

omega_variable(
    victim_set_emergence,
    'Are there identifiable medieval speakers who are harmed by the continuity reading — forced to internalize a sense of linguistic inadequacy or excluded from literacy because their speech patterns do not match classical forms?',
    'Examination of medieval educational texts, teaching practices, and the historical record of who was literate and how they were trained. Analysis of whether monastic schools accommodated living speech patterns or enforced classical conformity as a barrier to entry.',
    'If significant harm or exclusion is documented (literacy training that shames native speech, deliberate suppression of evolved forms), the constraint rises in extractiveness and acquires a victim set — it becomes a snare. If monastic education accommodated evolved speech patterns and made literacy accessible, no victim set exists and extractiveness remains low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_emergence, empirical, 'Whether the continuity reading''s authorization of medieval forms masked or excluded harm to speakers of evolved Latin.').

omega_variable(
    committer_reading_ambiguity,
    'Is the continuity reading a description of how medieval Latin actually evolved (a historical claim), or a normative framework medieval scholars used to justify their linguistic choices (a committer-level framing)?',
    'Reconstruction of medieval metalinguistic discourse: what did medieval grammarians, glossators, and copyists CLAIM they were doing? Did they self-consciously frame their work as legitimate evolution, or did they claim to be preserving classical forms? Modern retrospective framing of medieval practice versus medieval self-understanding.',
    'If medieval scholars self-consciously framed their work as legitimate evolution, the continuity reading is not merely modern retrospect but was instantiated in medieval epistemic practice. If medieval scholars claimed to be preserving classical forms while actually innovating, the continuity reading is a modern rescue narrative overlaid on medieval practice — still true as history, but not as the reading that medieval practitioners consciously held.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_ambiguity, conceptual, 'Whether the continuity reading represents medieval self-understanding or modern retrospective framing of medieval practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 500, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t500, latin_correctness__continuity_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(lati_tr_t700, latin_correctness__continuity_reading, theater_ratio, 700, 0.08).
narrative_ontology:measurement(lati_tr_t900, latin_correctness__continuity_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(lati_tr_t1100, latin_correctness__continuity_reading, theater_ratio, 1100, 0.16).
narrative_ontology:measurement(lati_tr_t1300, latin_correctness__continuity_reading, theater_ratio, 1300, 0.2).
narrative_ontology:measurement(lati_tr_t1400, latin_correctness__continuity_reading, theater_ratio, 1400, 0.18).

% Extraction over time
narrative_ontology:measurement(lati_be_t500, latin_correctness__continuity_reading, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(lati_be_t700, latin_correctness__continuity_reading, base_extractiveness, 700, 0.12).
narrative_ontology:measurement(lati_be_t900, latin_correctness__continuity_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement(lati_be_t1100, latin_correctness__continuity_reading, base_extractiveness, 1100, 0.16).
narrative_ontology:measurement(lati_be_t1300, latin_correctness__continuity_reading, base_extractiveness, 1300, 0.17).
narrative_ontology:measurement(lati_be_t1400, latin_correctness__continuity_reading, base_extractiveness, 1400, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t500, latin_correctness__continuity_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(lati_su_t700, latin_correctness__continuity_reading, suppression_requirement, 700, 0.15).
narrative_ontology:measurement(lati_su_t900, latin_correctness__continuity_reading, suppression_requirement, 900, 0.18).
narrative_ontology:measurement(lati_su_t1100, latin_correctness__continuity_reading, suppression_requirement, 1100, 0.22).
narrative_ontology:measurement(lati_su_t1300, latin_correctness__continuity_reading, suppression_requirement, 1300, 0.26).
narrative_ontology:measurement(lati_su_t1400, latin_correctness__continuity_reading, suppression_requirement, 1400, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__continuity_reading, 0.12).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three structurally distinct constraint stories: continuity_reading (this file) treats medieval Latin as legitimate evolution; rupture_reading treats it as corruption of a fixed standard; hybrid_reading permits both in different domains. The three readings have substantially different ε values (low for continuity, moderate-high for rupture and hybrid's enforcement burden) and different victim sets (none for continuity, literacy-excluded speakers for rupture). They are linked by network.affects_constraints to enable contamination and family-level analysis. Each story instantiates one reading's ε-invariant frame; they do not describe 'different measurements of the same constraint.' The epsilon-invariance principle requires separate constraint stories because the readings make different claims about what counts as legitimate Latin, and those claims produce different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
