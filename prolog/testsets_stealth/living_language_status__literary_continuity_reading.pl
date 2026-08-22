% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary-Continuity Criterion for Living-Language Status (Hebrew Case)
 *   domain: sociolinguistic/cultural-national
 *
 * SUMMARY:
 *   The colloquial predicate 'a living language' decomposes, per the
 *   ε-invariance principle, into three structurally distinct claims; this
 *   file instantiates the literary-continuity reading: Hebrew counts as alive
 *   because new literary and intellectual work is continuously produced in
 *   it, whatever the nativity of its speakers — the Haskalah periodicals
 *   (Ha-Meassef, 1783, onward) and the modern Hebrew canon are the standing
 *   proof. The ε referent is the standing arrangement under contest — the
 *   literary-continuity criterion as it actually operated from the Haskalah
 *   public sphere through the Israeli literary establishment — assessed by
 *   this reading's own lights, never by the rival criteria. Structurally the
 *   criterion coordinated a real achievement: a scattered intelligentsia with
 *   no common spoken language built a functioning transnational republic of
 *   letters in a revived high register. The same definition, however,
 *   assigned the era's actual spoken Jewish languages — Yiddish above all,
 *   later the Mizrahi vernaculars — a vitality score of zero, transferring
 *   cultural authority to the literate few. Sibling readings
 *   (liturgical_preservation_reading, native_generation_reading) are separate
 *   constraint files linked through network.affects_constraints; the contest
 *   among them is recorded in the kernel_reading_contest omega, not folded
 *   into this constraint. KEY AGENTS (by structural relationship): -
 *   haskalah_periodical_editors: Agenda setter (organized/identity_locked) —
 *   runs the gatekeeping, sets the register - secular_hebrew_intellectuals:
 *   Primary beneficiary (organized/mobile) — collects cultural authority -
 *   yiddish_speaking_masses: Primary target (powerless/trapped) — bears
 *   definitional exclusion - mizrahi_vernacular_speakers: Secondary target
 *   (powerless/trapped) — bears heritage-language stigma -
 *   rabbinical_custodians: Displaced authority (organized/identity_locked) -
 *   hebrew_language_academy: Institutional administrator
 *   (institutional/constrained) - diaspora_hebrew_readership: Incidental
 *   beneficiary (moderate/constrained) - comparative_sociolinguists:
 *   Analytical observer — sees the full structure
 *
 * KEY AGENTS:
 *   - haskalah_periodical_editors: Agenda setter (organized/identity_locked) — edits the journals, enforces the register, fused with the project
 *   - secular_hebrew_intellectuals: Primary beneficiary (organized/mobile) — collects prestige, prizes, chairs, canon placement
 *   - yiddish_speaking_masses: Primary target (powerless/trapped) — daily language scored zero in the vitality ledger
 *   - mizrahi_vernacular_speakers: Secondary target (powerless/trapped) — heritage languages stigmatized against the literary standard
 *   - rabbinical_custodians: Displaced authority (organized/identity_locked) — lost public custody of the holy tongue
 *   - hebrew_language_academy: Institutional administrator (institutional/constrained) — codifies the standard the sphere runs on
 *   - diaspora_hebrew_readership: Incidental beneficiary (moderate/constrained) — small paying public sustaining the journals
 *   - comparative_sociolinguists: Analytical observer (analytical/analytical) — documents divergence from transmission-based verdicts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.38).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.45).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary-Continuity Criterion for Living-Language Status (Hebrew Case)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistic/cultural-national").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '304e53f0-2616-4f34-b359-f17e522e5c0e').
narrative_ontology:cs_kernel_codification('304e53f0-2616-4f34-b359-f17e522e5c0e', distributed).
narrative_ontology:cs_authority_grounding('304e53f0-2616-4f34-b359-f17e522e5c0e', expertise).
narrative_ontology:cs_interpretation_layer_present('304e53f0-2616-4f34-b359-f17e522e5c0e').
narrative_ontology:cs_reading_relation('304e53f0-2616-4f34-b359-f17e522e5c0e', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('304e53f0-2616-4f34-b359-f17e522e5c0e', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('304e53f0-2616-4f34-b359-f17e522e5c0e', foundational, productive_literacy_constitutes_vitality).
narrative_ontology:cs_axiom_status(productive_literacy_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('304e53f0-2616-4f34-b359-f17e522e5c0e', productive_literacy_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('304e53f0-2616-4f34-b359-f17e522e5c0e', foundational, native_transmission_not_required_for_life).
narrative_ontology:cs_axiom_status(native_transmission_not_required_for_life, holdable).
narrative_ontology:cs_axiom_grounding('304e53f0-2616-4f34-b359-f17e522e5c0e', native_transmission_not_required_for_life, empirically_contingent).
narrative_ontology:cs_axiom('304e53f0-2616-4f34-b359-f17e522e5c0e', secondary, canon_admission_confers_cultural_authority).
narrative_ontology:cs_axiom_status(canon_admission_confers_cultural_authority, holdable).
narrative_ontology:cs_axiom_grounding('304e53f0-2616-4f34-b359-f17e522e5c0e', canon_admission_confers_cultural_authority, conventional).
narrative_ontology:cs_reference_frame('304e53f0-2616-4f34-b359-f17e522e5c0e', haskalah_literary_public_sphere).
narrative_ontology:cs_drift_state('304e53f0-2616-4f34-b359-f17e522e5c0e', contemporary_vitality_metrics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('304e53f0-2616-4f34-b359-f17e522e5c0e', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, haskalah_periodical_editors).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_hebrew_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, yiddish_speaking_masses).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, mizrahi_vernacular_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, diaspora_hebrew_readership).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, rabbinical_custodians).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_continuity_vitality_thesis).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, classical_corpus_modern_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit and finance the Hebrew-language journals from Ha-Meassef (1783) onward, decide which prose and verse counts as proper literary Hebrew, reject submissions in purely rabbinic or heavily Europeanized registers, and correspond across cities to hold contributors to a shared standard. Their reputations and livelihoods are bound to the journals' survival; stepping away means abandoning the project their identities were built around.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, haskalah_periodical_editors, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, haskalah_periodical_editors, beneficiary).

% Writers, poets, critics, and later university teachers who publish new work in Hebrew and collect the prestige, prizes, chairs, and canon placement that flow to acknowledged contributors. Many could have written in German, Russian, or English, and some did; those who stayed built careers on Hebrew alone.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_hebrew_intellectuals, beneficiary,
    organized, biographical, mobile, continental).

% Millions of Jews across Eastern Europe whose daily language was Yiddish. Under a criterion that counts only new literary work in Hebrew toward the language's life, their speech registers as nothing in the vitality ledger; their schools, theaters, and press are read as symptoms of linguistic poverty rather than as a parallel living culture. They held no seat in the periodical sphere that delivered the verdict.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, yiddish_speaking_masses, payer,
    powerless, immediate, trapped, regional).

% Jews from North Africa and the Middle East speaking Judeo-Arabic, Ladino, Persian, and related vernaculars who arrived in the Hebrew-speaking state mid-century. Their home languages were stigmatized as diaspora debris while literary Hebrew stood as the nation's authentic voice; adults were routed into intensive Hebrew absorption and children were discouraged from heritage speech.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, mizrahi_vernacular_speakers, payer,
    powerless, biographical, trapped, regional).

% Traditional scholars for whom Hebrew is the holy tongue sustained through prayer, Talmud study, and commentary. They opposed secular repurposing of the sacred register and watched custodial authority over the language pass to the literate secular class; their own institutions continue undisturbed, but the public meaning of the language moved away from them.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, rabbinical_custodians, payer,
    organized, civilizational, identity_locked, global).

% The statutory body that codifies grammar, coins terminology, and rules on correctness. It administers the standard the literary sphere runs on, and its mandate presupposes that Hebrew is a continuously developing language worth legislating for.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, hebrew_language_academy, agenda_setter,
    institutional, generational, constrained, national).

% A small, devoted public of subscribers and self-taught readers across the diaspora who bought the journals, kept them financially afloat, and gained access to modern science and literature in a Jewish idiom. Their numbers were always modest relative to the movement's claims about them.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, diaspora_hebrew_readership, beneficiary,
    moderate, biographical, constrained, continental).

% Researchers who classify languages by intergenerational transmission and domains of use. They note that the literary criterion answers a different question than theirs and document where the two verdicts diverge.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, comparative_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, secular_hebrew_intellectuals).
narrative_ontology:fixing_cost_class(living_language_status__literary_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a transnational literary public sphere in a standardized written register: geographically dispersed readers and writers who share no spoken vernacular obtain a common medium for intellectual exchange, canon formation, and cultural continuity.
% TRANSFER_FUNCTION: Moves cultural authority, publication resources, and definitional power over the language from traditional custodians and everyday speakers to the literate elite; places the labor of literacy on writers and readers, who must master the elevated register to participate at all.
% ABSENT_VOICES: Yiddish-speaking women, workers, and the vernacular-reading public were never consulted on what counts as the language's life — they sat outside the periodical subscription base and the editorial correspondence networks. Traditionalist laity who understood vitality liturgically were likewise outside the secular journals. Later, Mizrahi heritage speakers entered a state whose linguistic hierarchy had already been fixed without them.
% DISAPPEARANCE_RATIONALE: Periodicals, prizes, academy mandates, school curricula, and the elite's standing all cite the criterion. Overnight removal would force every vitality claim onto rival criteria, redistribute canon power and translation resources, and reopen the language question that the sibling readings answer differently — the arrangements built on this definition would visibly rearrange.
% FOUNDING_PROBLEM: A people dispersed across mutually unintelligible vernaculars needed a shared medium for modern intellectual life outside both the rabbinic curriculum and the surrounding state languages; the ancient Hebrew corpus was the only common resource available, and it had to be made to carry contemporary thought.
% FOUNDING_PROBLEM_CORROBORATION: Haskalah historiography independent of the benefiting parties — editors' prospectuses, correspondence, and subsequent academic histories — attests that the medium problem was real and urgent. No party outside the literary establishment attests that the literary criterion remains the operative measure of vitality today: contemporary vitality assessment rests on transmission metrics, so continued liveness of the founding problem is attested mainly from within the benefiting set.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type is tangled_rope: the criterion performs real coordination — a dispersed intelligentsia with no common spoken vernacular obtained a shared high register, solving a genuine collective-action problem — while the same definition assigns zero vitality-value to the speech of the non-literary majority, an asymmetry that requires active maintenance (editorial rejection, canon policing, curriculum control) to hold. Metrics are authored independently of the claim. Extractiveness 0.38 reflects status-and-resource costs borne by vernacular speakers rather than material confiscation. Suppression 0.45 is the raw structural force of gatekeeping and social sanction; it is authored unscaled — directionality and scope scaling happen in the engine, not here. Theater 0.30: most literary production is real, but vitality-demonstration ceremonies and thin-circulation flagship journals carry a growing performative share. Accessibility collapse is low (0.40) because rival definitions of vitality remained fully articulable throughout — the sibling readings are the standing proof. Resistance 0.55 records the Yiddishist counter-movement, traditionalist objection, and later heritage-language advocacy. The measurement series share one grid (t=0..240 at 40-year steps, every tracked metric authored at every point): extractiveness climbs as gatekeeping hardens through the language wars, peaks during state-building when the criterion supplied cover for marginalizing diaspora vernaculars, then eases once Hebrew became everyone's mother tongue and the criterion lost its monopoly on the vitality question; suppression traces the same arc; theater creeps upward as demonstration begins to outrun need.
 *
 * PERSPECTIVAL GAP:
 *   From the editor's chair the arrangement is a medium they built, funded, and defended — coordination experienced as creation. From a Yiddish-speaking household the same arrangement is a verdict delivered about their life without their consultation. The academy experiences mandate; the readership experiences access; the sociolinguist sees the divergence between the seats. Per-seat classifications are computed by the engine from these structural positions; nothing here adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Editors and secular intellectuals sit near the beneficiary pole: the definition routes authority and resources to exactly their practice, and the intellectuals' mobile exit (they could have written in German or Russian) keeps them from full subsidy only in the sense that their participation is chosen. Yiddish-speaking masses and Mizrahi vernacular speakers sit near the target pole, amplified by trapped exit — one cannot cheaply exit one's mother tongue. Rabbinical custodians are deliberately NOT declared victims: they are highly literate and their liturgical practice continues untouched, so the structural derivation will place them near-symmetric via the power-atom fallback; this likely understates their loss of public authority, but the override mechanism's granularity (by power atom) would misapply to the editors who share that atom, so the residual error is documented rather than patched. The academy sits near-symmetric-low as administrator; the readership sits mildly beneficiary-side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a common modern medium for a dispersed people — was real and externally corroborated, but it was ultimately solved by a route this reading did not center: mother-tongue transmission. Founding_problem_status is therefore contested, not dead: literary institutions still live off the criterion while its original justificatory burden has migrated to ground the native-generation reading occupies. The contested-status × world-rearranges cell keeps the zombie flag off, but the post_revival_redundancy omega tracks the drift path toward inertial maintenance: if vitality-demonstration becomes ceremony, theater rises and the arrangement slides piton-ward. The classification discipline cuts both ways: reading the arrangement as pure extraction erases the genuine transnational coordination the periodicals performed; reading it as pure coordination erases the definitional exclusion that fell on vernacular speakers. Mandatrophy resolution here means holding both facts in one type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the living_language_status kernel governs vitality verdicts in a given institution, and does this literary_continuity_reading remain one live reading among siblings rather than the settled definition?',
    'Track criterion adoption across vitality-assessment bodies (UNESCO vitality scales, Ethnologue-style classifications, national academies, ministries of culture, literary institutions); the sibling files liturgical_preservation_reading and native_generation_reading carry the rival criteria.',
    'If native_generation_reading captures official assessment, this reading''s operative scope shrinks to literary self-description and its measured reach contracts; if this reading prevails in cultural ministries, its extraction surface widens beyond the literate sphere.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This story is one reading of a contested kernel; sibling readings instantiate different constraints with different victim sets and different epsilon.').

omega_variable(
    vernacular_stigma_attribution,
    'How much of the stigma borne by Yiddish and Mizrahi vernacular speakers is attributable to the literary-continuity criterion itself, versus the nationalist political programs that invoked it?',
    'Compare periods and polities where the literary criterion operated without state enforcement (Haskalah diaspora networks) against state-enforced eras (Yishuv, Mandate, and Israeli language policy); isolate the criterion''s independent definitional work from the enforcement machinery of the programs that borrowed it.',
    'If attribution shifts to the political programs, this story''s suppression and effective extraction fall and the policy-level stories carry the coercive load; if the criterion did independent definitional work, the authored values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_stigma_attribution, conceptual, 'Attribution of vernacular stigma between the criterion and the political programs that wielded it.').

omega_variable(
    post_revival_redundancy,
    'After Hebrew acquired generations of native speakers, does the literary-continuity criterion still perform independent justificatory work, or does it persist as ceremonial self-description of the literary establishment?',
    'Content-analyze vitality claims in literary-institution discourse after statehood: do institutions cite literary productivity where transmission metrics would suffice, and do they fund vitality-demonstration activity (anniversaries, canon campaigns, prestige translations) beyond functional need?',
    'If redundant, theater_ratio should be revised upward and the arrangement drifts toward inertial maintenance; if the criterion still steers resource allocation (curricula, translation budgets, prize structures), it retains functional content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_revival_redundancy, empirical, 'Whether the criterion retains independent function after the native-transmission fact made its original argument unnecessary.').

omega_variable(
    gain_capture_seat,
    'Do the arrangement''s gains concentrate in a named seat — the literate elite — or diffuse across the wider literate public and the national culture?',
    'Follow prestige flows: prizes, academic chairs, canon admission, translation subsidies, and editorial income — identify who collects them relative to who funds and staffs the system.',
    'Concentration in the secular_hebrew_intellectuals seat supports the tangled-rope reading with an identifiable capturer; genuine diffusion would soften the asymmetry and push assessment toward ordinary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_seat, empirical, 'Whether gains accrue to a capturable seat or diffuse beyond the named stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_lit_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(lls_lit_tr_t40, living_language_status__literary_continuity_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(lls_lit_tr_t80, living_language_status__literary_continuity_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(lls_lit_tr_t120, living_language_status__literary_continuity_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(lls_lit_tr_t160, living_language_status__literary_continuity_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement(lls_lit_tr_t200, living_language_status__literary_continuity_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(lls_lit_tr_t240, living_language_status__literary_continuity_reading, theater_ratio, 240, 0.3).

% Extraction over time
narrative_ontology:measurement(lls_lit_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lls_lit_be_t40, living_language_status__literary_continuity_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(lls_lit_be_t80, living_language_status__literary_continuity_reading, base_extractiveness, 80, 0.33).
narrative_ontology:measurement(lls_lit_be_t120, living_language_status__literary_continuity_reading, base_extractiveness, 120, 0.41).
narrative_ontology:measurement(lls_lit_be_t160, living_language_status__literary_continuity_reading, base_extractiveness, 160, 0.46).
narrative_ontology:measurement(lls_lit_be_t200, living_language_status__literary_continuity_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(lls_lit_be_t240, living_language_status__literary_continuity_reading, base_extractiveness, 240, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(lls_lit_su_t0, living_language_status__literary_continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lls_lit_su_t40, living_language_status__literary_continuity_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(lls_lit_su_t80, living_language_status__literary_continuity_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(lls_lit_su_t120, living_language_status__literary_continuity_reading, suppression_requirement, 120, 0.58).
narrative_ontology:measurement(lls_lit_su_t160, living_language_status__literary_continuity_reading, suppression_requirement, 160, 0.52).
narrative_ontology:measurement(lls_lit_su_t200, living_language_status__literary_continuity_reading, suppression_requirement, 200, 0.47).
narrative_ontology:measurement(lls_lit_su_t240, living_language_status__literary_continuity_reading, suppression_requirement, 240, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, information_standard).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, native_generation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'living language' (epsilon-invariance): the label conflates three structurally distinct claims. This file is the literary-continuity reading (vitality = ongoing productive literary work; epsilon low-moderate, extraction is definitional and status-based). liturgical_preservation_reading (vitality = continuous ritual recitation and study) carries near-zero material extraction but its own authority contests. native_generation_reading (vitality = mother-tongue transmission) centers a different victim set (heritage speakers facing enforcement) and different beneficiaries (transmission institutions). The literary reading is upstream of the native reading historically — revival ideology cited the literary record as evidence that Hebrew could live again — so this file links to both siblings; each sibling links back per its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
