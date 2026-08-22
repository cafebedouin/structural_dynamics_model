% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__modernization_reading, []).

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
 *   constraint_id: orthographic_kernel__modernization_reading
 *   human_readable: Latin-Script Orthographic Standard of Turkish (Modernization Reading)
 *   domain: political/linguistic/commitment_systems
 *
 * SUMMARY:
 *   In 1928 the Turkish Republic replaced the Arabic-derived Ottoman script
 *   with a phonemically adapted Latin alphabet, enforced by statute (Law No.
 *   1353), compulsory adult literacy campaigns (Millet Mektepleri),
 *   examinations for civil servants, and progressive bans on Arabic-script
 *   publication. This story instantiates ONE reading of that standing
 *   arrangement — the modernization_reading: the Latin standard as an
 *   instrument that enabled technological and scientific modernization while
 *   preserving Turkish linguistic identity. Per the epsilon-invariance
 *   discipline, the referent is the standing Latin-script arrangement itself,
 *   assessed by this reading's own lights; the continuity_reading (Arabic
 *   script as Ottoman/Islamic continuity) and the rupture_reading (script
 *   change as deliberate cultural severance) are separate constraints
 *   authored in their own files and linked through
 *   network.affects_constraints. The modernization reading authors moderate
 *   epsilon: it credits the reform's genuine coordination gains (phonemic
 *   fit, literacy expansion, print/telegraph/administrative standardization)
 *   while acknowledging real extraction — the overnight devaluation of an
 *   entire class's literacy capital, compulsory transition burdens, and the
 *   legal suppression of the alternative script. The claim and the metrics
 *   are authored independently: the claimed type states what this reading
 *   holds to be structurally true, and the metrics state what is
 *   descriptively true of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - republican_state_bureaucracy: agenda setter and primary beneficiary (institutional/arbitrage) — wrote, administers, and collects from the orthographic standard
 *   - new_latin_literate_class: beneficiary (moderate/constrained) — holds the literacy capital the standard denominates
 *   - ottoman_literate_elites: primary target (moderate/identity_locked) — literacy capital devalued by statute
 *   - islamic_scholarly_establishment: target (organized/identity_locked) — Turkish-language religious textual tradition cut off from new readers
 *   - rural_adult_learners: coerced transition population, dual payer/beneficiary (powerless/trapped)
 *   - ottoman_script_print_trade: excluded (moderate/constrained) — Arabic-script printing capital written off without a seat in standard-setting
 *   - orthography_historians: analytical observer — sees the phonological fix, the capital destruction, and the identity settlement together
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, 0.48).
domain_priors:suppression_score(orthographic_kernel__modernization_reading, 0.4).
domain_priors:theater_ratio(orthographic_kernel__modernization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "Latin-Script Orthographic Standard of Turkish (Modernization Reading)").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political/linguistic/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, '7909bd0c-247a-479d-bf66-87dcce9793e8').
narrative_ontology:cs_kernel_codification('7909bd0c-247a-479d-bf66-87dcce9793e8', formalized).
narrative_ontology:cs_authority_grounding('7909bd0c-247a-479d-bf66-87dcce9793e8', extraction).
narrative_ontology:cs_interpretation_layer_present('7909bd0c-247a-479d-bf66-87dcce9793e8').
narrative_ontology:cs_reading_relation('7909bd0c-247a-479d-bf66-87dcce9793e8', orthographic_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('7909bd0c-247a-479d-bf66-87dcce9793e8', orthographic_kernel__rupture_reading, influences).
narrative_ontology:cs_axiom('7909bd0c-247a-479d-bf66-87dcce9793e8', foundational, script_as_instrument_of_modernization).
narrative_ontology:cs_axiom_status(script_as_instrument_of_modernization, holdable).
narrative_ontology:cs_axiom_grounding('7909bd0c-247a-479d-bf66-87dcce9793e8', script_as_instrument_of_modernization, instrumental).
narrative_ontology:cs_axiom('7909bd0c-247a-479d-bf66-87dcce9793e8', foundational, turkish_identity_carried_by_language_not_script).
narrative_ontology:cs_axiom_status(turkish_identity_carried_by_language_not_script, holdable).
narrative_ontology:cs_axiom_grounding('7909bd0c-247a-479d-bf66-87dcce9793e8', turkish_identity_carried_by_language_not_script, empirically_contingent).
narrative_ontology:cs_reference_frame('7909bd0c-247a-479d-bf66-87dcce9793e8', phonemic_latin_standard_as_modern_instrument).
narrative_ontology:cs_drift_state('7909bd0c-247a-479d-bf66-87dcce9793e8', contemporary_reottomanization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7909bd0c-247a-479d-bf66-87dcce9793e8', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, republican_state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_latin_literate_class).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, ottoman_literate_elites).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, islamic_scholarly_establishment).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, rural_adult_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, rural_adult_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote and administers the 1928 alphabet law through the Ministry of National Education and, later, the Turkish Language Association. Runs the compulsory adult literacy campaigns, examines and certifies civil servants and professionals in the new script, decides which script may appear in print, official records, and schoolbooks, and controls the transcription of the pre-1928 written corpus. The single standard gives it uniform records across provinces, workable telegraph and print administration, and gatekeeping authority over the nation's textual past. Its own position is the standard: it can amend the orthography at will and bears no penalty for doing so.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, republican_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Teachers, clerks, journalists, and students formed in the new alphabet. The reform opened positions — village schools, print shops, government offices — that only Latin-script literacy could fill, and their careers and reading worlds are denominated in the standard. Leaving the standard would mean abandoning the skill their livelihoods rest on; their stake is real, but they did not set the terms and cannot change them.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_latin_literate_class, beneficiary,
    moderate, biographical, constrained, national).

% Writers, poets, senior officials, and scholars trained in Ottoman script over decades. The statute made their reading and writing skill unusable in public life within a few years: Arabic-script publication was phased out, the living circulation of their libraries ended, and mid-career retraining meant learning to read and write again from scratch. Their life's formation and social standing are bound to the displaced script; private writing and religious study are the only remaining venues for it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ottoman_literate_elites, payer,
    moderate, generational, identity_locked, national).

% Ulema, mosque teachers, and Sufi orders whose teaching and commentary ran through Arabic-script texts. Core Arabic-language scriptures remained readable, but the Ottoman Turkish commentary and devotional literature that connected them to lay readers was cut off as new generations learned only the Latin alphabet. Religious instruction in the old script retreated to private homes and informal circles, outside the recognized school system.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, islamic_scholarly_establishment, payer,
    organized, generational, identity_locked, national).

% Adult villagers and townspeople, up to middle age, required to attend evening Nation's Schools classes and pass script examinations on pain of fines and, for officials, dismissal. They bore the transition's immediate burden — hours after full workdays, examination anxiety, the humiliation of functional illiteracy in their own language — and many gained literacy they had never had. Attendance was compulsory; there was no individual way to decline.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, rural_adult_learners, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, rural_adult_learners, beneficiary).

% Printers, typefounders, compositors, and publishers whose presses, type stock, and skills were built for Arabic-script composition. As Arabic-script printing was restricted, their equipment and market evaporated; retooling to Latin type meant writing off sunk capital. They had no seat in the Language Council or the ministry deliberations that decided the standard their trade depended on.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ottoman_script_print_trade, excluded,
    moderate, biographical, constrained, national).

% Comparative linguists and historians of script reform who study Turkey alongside Romania's 1860s transition and the Soviet latinization campaigns. They can see the phonological problem the reform fixed, the skill and capital its enforcement destroyed, and the identity question it settled by statute rather than argument, without holding any of the positions at stake.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, orthography_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__modernization_reading, republican_state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_kernel__modernization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single phonemic Latin orthography for Turkish solved a real coordination problem: the Arabic script's vowel poverty mismatched Turkish phonology, slowing literacy acquisition and making printing, telegraphy, and administrative record-keeping costly and error-prone; one standard script coordinated schooling, administration, publishing, and later machine encoding.
% TRANSFER_FUNCTION: Moves literacy capital and textual authority: devalues the accumulated Arabic-script reading and writing skill of Ottoman-trained elites, transfers standard-setting authority, print infrastructure, and control over transcription of the pre-1928 corpus to the state and its language institutions, and shifts the burden of retraining onto the whole literate population — most acutely onto adults compelled into evening classes.
% ABSENT_VOICES: Ottoman-script publishers and typefounders had no seat in the Language Council or ministry deliberations; religious authorities objecting to the displacement of the Arabic script were heard and overruled, then progressively excluded as Arabic-script publication was restricted; ordinary adults subject to compulsory classes were consulted through neither ballot nor hearing — their consent was presumed by the revolutionary vanguard.
% DISAPPEARANCE_RATIONALE: If the mandatory Latin standard and its enforcement vanished overnight, Turkish administration, schooling, and publishing would fragment across two scripts; a century of records, curricula, and machine-readable text is denominated in the Latin standard, and the state's textual infrastructure — including its gatekeeping over the Ottoman corpus — would dissolve into contested transcription markets.
% FOUNDING_PROBLEM: In the late Ottoman and early Republican period, Turkish written in the Arabic script was phonologically mismatched (a vowel-poor script for a vowel-rich language), mass illiteracy stood near ninety percent, and the state sought rapid literacy expansion plus direct access to Western scientific, technical, and print infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by: comparative orthography research on script-phonology fit and literacy acquisition (linguists with no stake in the Turkish standard); independent literacy statistics from the Turkish Statistical Institute and UNESCO series; and the recorded testimony of Ottoman-trained writers and clerics — contemporaries outside the state who attest the founding problem was real while disputing that solving it required the arrangement's permanent enforcement machinery. No corroborating source attests that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__modernization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__modernization_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__modernization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__modernization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.48 (moderate, matching the reading's expected delta): the arrangement solves a real coordination problem — Arabic script under-represented Turkish's eight vowels, slowing literacy acquisition and burdening administrative print — but the same statute that installed the standard devalued the accumulated Arabic-script capital of Ottoman literates, compelled rural adults into evening schools under penalty, and legally collapsed the alternative. Suppression 0.40: active enforcement (publication bans, compulsory examinations, dismissals) decayed after compliance was achieved, but the legal prohibition on the alternative was never repealed. Theater 0.28: the literacy campaigns were functionally real; a modest commemorative layer (Alphabet Revolution anniversaries, official rhetoric) accumulated as the functional campaign wound down. Accessibility collapse 0.65: the alternative collapsed in public space (print, administration, schooling) but persisted privately in religious instruction and personal writing. Resistance 0.45: parliamentary dissent, literary holdouts, and religious opposition were real and prosecuted, but overwhelmed within a decade. The three measurement series share one time grid (1928-1958, seven points each) so the engine samples every metric at every authored point. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by directionality and scope downstream. The declining suppression_requirement series models enforcement normalization, not liberalization of principle — see the suppression_decay_mechanism omega.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state bureaucracy) experiences the arrangement as a self-built instrument: it wrote the standard, runs it, and collects its administrative rents, so its computed classification should sit near the coordination end. The identity-locked payer seats (Ottoman literates, ulema) experience the same statute as overnight destruction of capital they cannot exit without abandoning their life's formation. The new literate class sits between: it collects the positions the reform opened, but its capital is bound to a standard it did not set. Rural adult learners hold both a payer seat (compulsory attendance under penalty) and a beneficiary seat (the literacy the reform delivered). Note also that the identity-locked victim seats never coalesced: secular Ottoman literates, the ulema, and rural learners were politically fragmented and never mounted a joint challenge, which is why moderate-power and organized victims nonetheless failed to move the arrangement. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The bureaucracy is declared beneficiary and agenda setter — derivation places it near the beneficiary end of d (low effective extraction). The new literate class is beneficiary with constrained exit — low-to-moderate d. The Ottoman literate elites and the scholarly establishment are declared victims with identity-locked exit — derivation places them near the full-target end (high effective extraction): their costs are amplified because they cannot arbitrage away from a devalued identity formation. Rural adult learners are victims with trapped exit but a genuine secondary benefit — mid-to-high d. No directionality overrides are needed: the beneficiary/victim declarations plus exit options produce the correct ordering without correction. Spatial scope is national across the enforcing seats, which modestly amplifies effective extraction relative to a local standard; the excluded print trade and the analytical observer feed no directionality arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk runs in both directions. Read as pure rope (the official framing), the story would erase the expropriation — the statute did not merely coordinate, it destroyed one class's capital to build another's. Read as pure snare (the rupture reading's temptation), it would erase the genuine coordination function — Turkish literacy demonstrably accelerated under a phonemic script, and no serious actor proposes reverting. The tangled_rope claim holds both: coordination function real, extraction real, active enforcement required to hold the asymmetry. On the R5 interview: the founding problem (phonological mismatch, mass illiteracy, technological disconnection) was live at founding and is substantially addressed — literacy rose from roughly one in ten toward near-universality over the century — but the founding problem's status is now contested: beneficiaries cite ongoing standardization and technology adaptation; critics note the acute transition completed generations ago and what persists is a settled monopoly plus suppression of the alternative. The status=contested by verdict=world_rearranges cell is exactly the configuration the mismatch check watches: a world-rearranging arrangement whose founding warrant is no longer uncontested, with the arrangement's gains accruing to a named seat (the bureaucracy) and fixing priced as prohibitive — the cost-asymmetry that keeps the standing arrangement in place regardless of whether its warrant still holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the modernization_reading of orthographic_kernel; how would the continuity_reading or the rupture_reading, held over the same standing Latin-script arrangement, re-author epsilon, the beneficiary/victim structure, and the classification? The readings'' disagreement is located in the FUNCTION attributed to the same act — instrument (this reading), inheritance-carrier (continuity), identity-act (rupture) — which propagates into epsilon and the victim set.',
    'Author the two sibling stories over the same fixed referent (the standing Latin-script arrangement) and compare per-seat classifications and epsilon values; divergence between the three files locates the disagreement structurally rather than rhetorically.',
    'The continuity_reading would raise epsilon (it reads the arrangement as expropriation of a living Ottoman/Islamic textual tradition) and extend the victim set to all Ottoman-script readers; the rupture_reading would raise epsilon further and recode the coordination story as cover for deliberate identity engineering, shifting the agenda-setter seat from administrator to author of cultural severance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is one of three readings of the orthographic kernel; sibling readings change the structural assessment of the same arrangement.').

omega_variable(
    transition_cost_allocation,
    'Who actually bore the literacy-transition costs and who captured the standard-setting gains — was the allocation asymmetric enough that the arrangement''s costs exceed its inherent coordination cost?',
    'Historical-demographic study of Nation''s Schools attendance, civil-service examination dismissals, and Arabic-script publication cessations, cross-referenced against who staffed the new administration, the retooled print trade, and the expanded teaching corps.',
    'If costs fell disproportionately on Ottoman-literate elites and rural adults while standard-setting rents accrued to the bureaucracy and the new literate class, epsilon sits at the high end of moderate; if costs were broadly shared across the literate population, epsilon approaches rope territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_allocation, empirical, 'Distribution of transition costs versus standard-setting gains across seats.').

omega_variable(
    identity_persistence_status,
    'Is ''Turkish identity persisted through the script change because identity is carried by language, not script'' an empirical finding the reform vindicated, or a definitional convention internal to this reading?',
    'Comparative analysis of script transitions that did versus did not coincide with identity rupture (Romanian 1860s, Soviet latinization and subsequent cyrillization, contemporary Kazakh debates), plus longitudinal identity and literacy surveys.',
    'If identity persistence is definitional rather than demonstrated, this reading''s second foundational axiom weakens and the rupture_reading''s account of the same events gains structural ground; if demonstrated, the axiom is empirically load-bearing and the reading is stabilized against its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_persistence_status, conceptual, 'Whether the identity-preservation clause is a vindicated empirical claim or the reading''s own framing convention.').

omega_variable(
    suppression_decay_mechanism,
    'Does the declining suppression trajectory reflect genuine normalization (the alternative ceased to matter on its own terms) or suppression-by-success (the alternative was eliminated, so enforcement became unnecessary)?',
    'Observe enforcement behavior when the alternative revives — Ottoman-script electives, religious publishing, and neo-Ottoman cultural production since the 1990s: renewed legal friction indicates suppression-by-success; indifferent tolerance indicates genuine normalization.',
    'Suppression-by-success means the standing arrangement retains high latent suppression and the low measured value understates its coercive structure; genuine normalization supports the lower suppression reading and a softer classification at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_decay_mechanism, empirical, 'Whether enforcement decay reflects normalization or completed elimination of the alternative script.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 1928, 1958).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__modernization_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(orth_tr_t1933, orthographic_kernel__modernization_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement(orth_tr_t1938, orthographic_kernel__modernization_reading, theater_ratio, 1938, 0.18).
narrative_ontology:measurement(orth_tr_t1943, orthographic_kernel__modernization_reading, theater_ratio, 1943, 0.21).
narrative_ontology:measurement(orth_tr_t1948, orthographic_kernel__modernization_reading, theater_ratio, 1948, 0.24).
narrative_ontology:measurement(orth_tr_t1953, orthographic_kernel__modernization_reading, theater_ratio, 1953, 0.26).
narrative_ontology:measurement(orth_tr_t1958, orthographic_kernel__modernization_reading, theater_ratio, 1958, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__modernization_reading, base_extractiveness, 1928, 0.62).
narrative_ontology:measurement(orth_be_t1933, orthographic_kernel__modernization_reading, base_extractiveness, 1933, 0.58).
narrative_ontology:measurement(orth_be_t1938, orthographic_kernel__modernization_reading, base_extractiveness, 1938, 0.55).
narrative_ontology:measurement(orth_be_t1943, orthographic_kernel__modernization_reading, base_extractiveness, 1943, 0.52).
narrative_ontology:measurement(orth_be_t1948, orthographic_kernel__modernization_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(orth_be_t1953, orthographic_kernel__modernization_reading, base_extractiveness, 1953, 0.49).
narrative_ontology:measurement(orth_be_t1958, orthographic_kernel__modernization_reading, base_extractiveness, 1958, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__modernization_reading, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement(orth_su_t1933, orthographic_kernel__modernization_reading, suppression_requirement, 1933, 0.68).
narrative_ontology:measurement(orth_su_t1938, orthographic_kernel__modernization_reading, suppression_requirement, 1938, 0.58).
narrative_ontology:measurement(orth_su_t1943, orthographic_kernel__modernization_reading, suppression_requirement, 1943, 0.5).
narrative_ontology:measurement(orth_su_t1948, orthographic_kernel__modernization_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(orth_su_t1953, orthographic_kernel__modernization_reading, suppression_requirement, 1953, 0.42).
narrative_ontology:measurement(orth_su_t1958, orthographic_kernel__modernization_reading, suppression_requirement, 1958, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the 1928 Turkish script reform' covers three structurally distinct readings of one standing kernel — the Latin-script orthographic arrangement. Each reading authors its own epsilon over the fixed referent (the standing arrangement, never the reading's endorsed alternative): this file instantiates the modernization_reading (moderate epsilon: real coordination gains, real transition costs); orthographic_kernel__continuity_reading authors the same arrangement as expropriation of a living Ottoman/Islamic textual tradition (higher epsilon, victim set extended to all Ottoman-script readers); orthographic_kernel__rupture_reading authors it as deliberate identity engineering with the coordination story as cover (highest epsilon, coordination function recoded as instrument). The modernization reading is the officially institutionalized one and structurally shaped the operating environment of both siblings; the family is linked through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
