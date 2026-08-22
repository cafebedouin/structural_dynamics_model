% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__ottoman_continuity_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__ottoman_continuity_reading
 *   human_readable: Ottoman Continuity Reading: Arabic Script as the Legitimate Graphemic Substrate of Turkish
 *   domain: political linguistics/state formation/cultural engineering
 *
 * SUMMARY:
 *   In the late Ottoman and early Republican conjuncture (1908-1938), written
 *   Turkish ran on the Perso-Arabic script, taught through state schools and
 *   medreses, enforced through press laws and orthography standards, and
 *   defended by the clerical and scribal estates as the vessel of
 *   Islamic-Ottoman civilization. This story instantiates ONE reading of the
 *   contested kernel turkish_graphemic_substrate: the
 *   ottoman_continuity_reading, which holds Turkish linguistic identity
 *   continuous with Ottoman-Islamic civilization and the Arabic script as the
 *   legitimate graphemic substrate. The standing arrangement under contest —
 *   and therefore the epsilon referent — is the maintenance of that script as
 *   the sole legitimate orthography for Turkish in schooling, administration,
 *   and print. The arrangement carries a genuine coordination function
 *   (access to the Islamic-Ottoman corpus, a trans-imperial Muslim graphemic
 *   community, administrative continuity) and a genuine extraction structure
 *   (a multi-year orthography barrier that priced literacy beyond commoner
 *   reach and paid scarcity rents to the scribal and clerical estates). The
 *   sibling readings — secular_nationalist_reading and
 *   gradual_transition_reading — are separate constraints with their own
 *   epsilon, beneficiaries, and classifications; they are linked, not
 *   averaged, here. Claim/metric independence: claimed_type is authored from
 *   structural analysis; the metrics are authored from descriptive history;
 *   where they diverge from any computed seat classification, that divergence
 *   is the datum.
 *
 * KEY AGENTS:
 *   - - ulema_religious_establishment: Agenda-setting beneficiary (institutional / identity_locked) — administers religious education, declares the script inviolable, cannot exit without dissolving its own authority
 *   - - ottoman_scribal_bureaucracy: Beneficiary with administrative enforcement duties (powerful / constrained) — prices the scarcity of orthographic mastery; retraining would erase its premium
 *   - - anatolian_peasant_majority: Primary payer (powerless / trapped) — bears the multi-year literacy barrier; holds no seat in the arrangement
 *   - - reformist_intellectuals: Excluded challenger (organized / mobile) — campaigns for Latin letters under press-law prosecution; exits into exile
 *   - - pan_islamic_press_circle: Secondary beneficiary (organized / mobile) — the shared script is its trans-imperial readership network
 *   - - ottoman_state_education_ministry: Enforcement host (institutional / trapped) — supplies the coercive machinery; defects to the rival reading in 1928
 *   - - linguistic_historians: Analytical observer (analytical / analytical) — compares acquisition costs and corpus access across regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.62).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.6).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Ottoman Continuity Reading: Arabic Script as the Legitimate Graphemic Substrate of Turkish").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political linguistics/state formation/cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, 'ba529b4b-e182-4181-b189-917f7c35093a').
narrative_ontology:cs_kernel_codification('ba529b4b-e182-4181-b189-917f7c35093a', formalized).
narrative_ontology:cs_authority_grounding('ba529b4b-e182-4181-b189-917f7c35093a', lineage).
narrative_ontology:cs_interpretation_layer_present('ba529b4b-e182-4181-b189-917f7c35093a').
narrative_ontology:cs_reading_relation('ba529b4b-e182-4181-b189-917f7c35093a', turkish_graphemic_substrate__secular_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ba529b4b-e182-4181-b189-917f7c35093a', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('ba529b4b-e182-4181-b189-917f7c35093a', foundational, ottoman_islamic_identity_continuity).
narrative_ontology:cs_axiom_status(ottoman_islamic_identity_continuity, holdable).
narrative_ontology:cs_axiom_grounding('ba529b4b-e182-4181-b189-917f7c35093a', ottoman_islamic_identity_continuity, deontological).
narrative_ontology:cs_axiom('ba529b4b-e182-4181-b189-917f7c35093a', foundational, quranic_script_inviolability).
narrative_ontology:cs_axiom_status(quranic_script_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('ba529b4b-e182-4181-b189-917f7c35093a', quranic_script_inviolability, theological).
narrative_ontology:cs_reference_frame('ba529b4b-e182-4181-b189-917f7c35093a', ottoman_islamic_graphemic_continuum).
narrative_ontology:cs_drift_state('ba529b4b-e182-4181-b189-917f7c35093a', post_1928_latinization_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ba529b4b-e182-4181-b189-917f7c35093a', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_religious_establishment).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_scribal_bureaucracy).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, pan_islamic_press_circle).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, anatolian_peasant_majority).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, reformist_intellectuals).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, islamic_ummah_graphemic_unity).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_civilizational_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the medreses, issue fetvas, and teach Quranic recitation in the Arabic script; their standing rests on being the indispensable mediators between believers and a scripture whose graphic form they declare inviolable. They pronounce script reform impious and organize opinion against it. Stepping outside the arrangement would dissolve the authority their training, office, and self-understanding are built on, so none of them can individually leave it.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_religious_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_religious_establishment, beneficiary).

% Fill the chanceries, courts, and upper schools of the empire; command of Ottoman orthography takes years to acquire and is the basis of their salaries, rank, and standing. They administer the orthography standards through the education ministry and the press censor's office. Some privately admire the efficiency of Latin letters, but retraining would erase the scarcity value of their skill, so their livelihoods are chained to the existing script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_scribal_bureaucracy, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_scribal_bureaucracy, agenda_setter).

% Farm and labor in villages where schools are distant and children's work is needed at home; the orthography demanded by the state schools requires years of study their households cannot spare, so roughly nine in ten remain unable to read anything. Their daughters are nearly all excluded. They bear the arrangement's costs in foregone literacy and hold no seat in any council that administers it.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, anatolian_peasant_majority, payer,
    powerless, immediate, trapped, local).

% Journalists, teachers, and officers who publish arguments for adopting Latin letters and simplifying spelling; under the press laws their advocacy is fined, banned, or driven into exile in Cairo, Geneva, and Paris. They are themselves products of the old script's culture, yet they absorb the arrangement's coercive edge whenever they campaign against it, and they hold no office in the boards that set orthography policy.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, reformist_intellectuals, excluded,
    organized, biographical, mobile, national).

% Edit and write for journals in Istanbul and Cairo addressed to a Muslim readership stretching from the Balkans to India; the shared Arabic-letter medium is their distribution network and their claim to speak for the ummah. They defend the script editorially as the bond of Islamic civilization and gather subscribers and influence from its maintenance.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, pan_islamic_press_circle, beneficiary,
    organized, generational, mobile, continental).

% Runs the school system, prescribes the orthography primers, licenses newspapers, and prosecutes violations of the press laws. Its enforcement capacity is what keeps the single-script standard operative across the provinces; after 1928 its successor ministry turns the same machinery against the old script, fining its public use and retraining teachers within months.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_state_education_ministry, agenda_setter,
    institutional, generational, trapped, national).

% Compare acquisition times, archive continuity, and corpus access across the script regimes from outside any of the contending camps; they take testimony from all sides and owe their standing to no faction's victory.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_scribal_bureaucracy).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single graphemic medium joining Turkish-speaking Muslims to the Quran, to six centuries of Ottoman administration and poetry, and to a wider Islamic lettered community from Sarajevo to Lucknow; standardizes official record-keeping across the empire's provinces.
% TRANSFER_FUNCTION: Moves years of scarce schooling and orthographic apprenticeship out of commoner households and concentrates reading, writing, and administrative voice in the clerical and scribal estates; moves the prestige of the Islamic corpus outward to all who can read it, while moving the cost of access onto those least able to pay it.
% ABSENT_VOICES: The illiterate majority itself — the people whose exclusion defines the arrangement — sits outside every council that administers it; village women, whose literacy rates were lowest, are doubly absent; artisans and shopkeepers who wanted practical literacy for trade petitioned through intermediaries at best. Present, they would ask for a script learnable in months, not years.
% DISAPPEARANCE_RATIONALE: Overnight removal of the single-script standard would reopen school curricula, invalidate chancery practice, free the press to print in any letters, and sever the religious classroom from the state school — the whole textual economy of the empire would reorganize around whatever script each community chose, which is what happened in reverse after the 1928 law.
% FOUNDING_PROBLEM: Bind the empire's Muslim population into one lettered community with unbroken access to the Islamic scripture and the accumulated Ottoman corpus, and give a multi-provincial administration a uniform written medium.
% FOUNDING_PROBLEM_CORROBORATION: Pan-Islamic circles outside the rent-collecting core — Indian Khilafatist writers and Ottoman exile journals in Cairo — attest the continuity problem as live, though they share the arrangement's ideological frame. Foreign educators and missionary literacy reports corroborate the cost side (the acquisition burden) while denying the necessity side. Stated plainly: no fully disinterested party attests that this particular script remains necessary; corroboration is partial and partially interested.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 for the arrangement's mature operating years: the orthography demanded five to seven years of schooling before functional literacy, against roughly half a year for a phonetic script, and that gap priced literacy beyond most households while paying scarcity rents to the scribal estate. Suppression (0.60) is a raw structural property, unscaled by power or scope: press-law prosecution of reform advocacy, a state monopoly on schooling, and the framing of script change as impiety. Theater is low (0.13) while the arrangement governs — the script carried real administrative, religious, and literary traffic; the series shows theater rising only as the arrangement loses its enforcement host after 1928, which is decay, not steady-state performance. Accessibility_collapse (0.48) reflects real but marginal alternatives — Karamanli Greek-letter Turkish, telegraph-era spelling simplifications, exile-printed Latin experiments — none able to scale inside the state system. Resistance (0.42) registers the reformist press, officer-corps sympathy, and periodic ministry memoranda rather than mass movement; the peasant majority's potential coalition power stayed latent because it was dispersed, seasonally bound, and unreachable by print. All three tracked series share one seven-point grid (1908, 1914, 1919, 1924, 1928, 1933, 1938); base_properties are anchored to the mature operating point (roughly 1919-1924), not to the terminal decay the series records.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the peasant and reformist seats the arrangement presents as enforced exclusion — costs concentrated, benefits remote, exit blocked or punishable. From the ulema and scribal seats the same structure presents as inheritance and livelihood — a coordination they staff and a corpus they keep. The ministry seat experiences the arrangement as an enforcement expenditure whose returns are order and legitimacy, until 1928 when the same machinery is redirected and the seat's experience inverts. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (ulema, scribal bureaucracy, pan-Islamic press) drive those seats toward the beneficiary end of directionality; the scribal seat's constrained exit keeps it somewhat above the pure-beneficiary floor because its premium depends on continued scarcity. Victim declarations (peasant majority, reformist intellectuals) drive those seats toward the target end; the peasants' trapped exit places them nearest full-target, while the reformists' mobility damps their effective extraction — they can carry their project abroad. The ulema's identity lock does not push them toward the target end because they are beneficiaries, not targets: the lock binds them to the arrangement's defense, which is where their low-d position is spent. The ministry seat carries no beneficiary or victim declaration and falls to its power-atom fallback; it is treated here as the enforcement host whose costs and legitimacy returns roughly balance.
 *
 * MANDATROPHY ANALYSIS:
 *   At the moment of contest the founding problem is live: the corpus exists, the trans-imperial readership exists, and the reading's holders sincerely administer it — so no mandatrophy resolution is declared, and the status=live x world_rearranges pairing raises no zombie flag. The terminal series nonetheless sketches a drift hypothesis rather than a verdict: after 1928 the arrangement survives mainly in private religious instruction and elderly correspondence, its theater ratio climbing past 0.5 while its extractiveness collapses — the signature of a residue maintained performatively by those who cannot exit it. Whether that residue is a degraded remnant in formation or a genuine minority practice is routed to the post_displacement_residue_status omega rather than asserted here. The tangled_rope claim itself guards both mislabels: pure-extraction coding would erase the corpus-coordination function that made the arrangement defensible to millions outside the rent-collecting seats, and pure-coordination coding would erase the literacy barrier that made it indefensible to the majority living under it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_resolution,
    'Which reading of the turkish_graphemic_substrate kernel governs the standing arrangement — and therefore whose epsilon, beneficiaries, and victims describe the operative constraint?',
    'Political settlement record: the 1928 Law on the Adoption of the Turkish Alphabet and subsequent enforcement resolved the contest in favor of the secular nationalist reading; cross-story comparison over the three linked files tracks the settlement.',
    'Under continuity governance this story''s classification stands as authored; under secular nationalist governance the same practices become a suppressed minority usage and the victim/beneficiary sets invert relative to the successor constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Committer-frame indexical: this constraint is one reading of a three-reading kernel; classification is reading-relative.').

omega_variable(
    literacy_barrier_attribution,
    'How much of the measured illiteracy under the arrangement is attributable to Arabic-script orthographic difficulty versus school scarcity, poverty, and seasonal labor demand?',
    'Comparative acquisition studies across script systems and within-Ottoman variation (communal schools, Karamanli and Ladino literacy rates) isolating the script effect from schooling supply.',
    'If script difficulty explains a minor share, extraction falls toward a coordination-cost reading and the rope component dominates; if a major share, the extraction estimate firms and the arrangement sits nearer the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_barrier_attribution, empirical, 'Attribution split between script difficulty and schooling supply in the literacy barrier.').

omega_variable(
    elite_gatekeeping_vs_conviction,
    'Did the scribal and clerical estates defend the script primarily to protect scarcity rents, primarily from sincere civilizational conviction, or in what mixture?',
    'Private correspondence, diaries, and recorded positions of chancery officials on successive reform memoranda; wage-premium analysis of scribal employment against reform proposals.',
    'A rent-dominant finding strengthens the extraction reading of beneficiary behavior; a conviction-dominant finding shifts weight to identity coordination and softens the capture attribution in gain_flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_gatekeeping_vs_conviction, conceptual, 'Motive mixture behind elite defense of the script.').

omega_variable(
    suppression_internalization_split,
    'Of the measured suppression, how much is structural (press laws, school monopoly, prosecution) and how much internalized (piety-fused conviction that abandoning the script abandons the faith)?',
    'Post-1928 trajectory: if deference to the old script persisted in private after prosecution ended, the internalized share is substantial; rapid private abandonment indicates structural dominance.',
    'A large internalized share raises the arrangement''s true hold over its payers beyond the legal record and predicts slower residue decay than enforcement decay alone implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the arrangement''s suppressive force.').

omega_variable(
    post_displacement_residue_status,
    'After 1928, does the displaced arrangement persist as a self-sustaining minority practice, decay to a purely theatrical residue, or extinguish within a generation?',
    'Literacy surveys of cohorts schooled before 1928, records of private Quranic instruction, and publication runs of Arabic-letter Turkish print through the 1940s-1960s.',
    'Persistence with rising theater supports a degraded-residue trajectory for this constraint; extinction closes the story as a terminated arrangement rather than a lingering one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_displacement_residue_status, empirical, 'Post-1928 survival mode of the displaced arrangement.').

omega_variable(
    ulema_identity_lock_counterfactual,
    'If the fusion of script and piety broke — for instance, through widespread acceptance of transliterated Qurans — would the ulema''s identity lock soften enough to change their seat''s computed position?',
    'Track ulema accommodation behavior where transliteration or translation gained ground (Republican-era Diyanet policies, diaspora communities).',
    'A softened lock would lower the cost of the ulema seat''s exit and reduce the arrangement''s defensive intensity; an intact lock predicts continued private-sphere maintenance regardless of legal status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ulema_identity_lock_counterfactual, conceptual, 'Durability of the identity lock binding the clerical estate to the script.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 1908, 1938).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t1908, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1908, 0.1).
narrative_ontology:measurement_basis(turk_tr_t1908, observed).
narrative_ontology:measurement(turk_tr_t1914, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1914, 0.11).
narrative_ontology:measurement_basis(turk_tr_t1914, observed).
narrative_ontology:measurement(turk_tr_t1919, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1919, 0.13).
narrative_ontology:measurement_basis(turk_tr_t1919, observed).
narrative_ontology:measurement(turk_tr_t1924, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1924, 0.17).
narrative_ontology:measurement_basis(turk_tr_t1924, observed).
narrative_ontology:measurement(turk_tr_t1928, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1928, 0.28).
narrative_ontology:measurement_basis(turk_tr_t1928, observed).
narrative_ontology:measurement(turk_tr_t1933, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1933, 0.44).
narrative_ontology:measurement_basis(turk_tr_t1933, observed).
narrative_ontology:measurement(turk_tr_t1938, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1938, 0.58).
narrative_ontology:measurement_basis(turk_tr_t1938, observed).

% Extraction over time
narrative_ontology:measurement(turk_be_t1908, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1908, 0.58).
narrative_ontology:measurement_basis(turk_be_t1908, observed).
narrative_ontology:measurement(turk_be_t1914, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1914, 0.61).
narrative_ontology:measurement_basis(turk_be_t1914, observed).
narrative_ontology:measurement(turk_be_t1919, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1919, 0.64).
narrative_ontology:measurement_basis(turk_be_t1919, observed).
narrative_ontology:measurement(turk_be_t1924, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1924, 0.63).
narrative_ontology:measurement_basis(turk_be_t1924, observed).
narrative_ontology:measurement(turk_be_t1928, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1928, 0.68).
narrative_ontology:measurement_basis(turk_be_t1928, observed).
narrative_ontology:measurement(turk_be_t1933, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1933, 0.52).
narrative_ontology:measurement_basis(turk_be_t1933, observed).
narrative_ontology:measurement(turk_be_t1938, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1938, 0.34).
narrative_ontology:measurement_basis(turk_be_t1938, observed).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t1908, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1908, 0.56).
narrative_ontology:measurement_basis(turk_su_t1908, observed).
narrative_ontology:measurement(turk_su_t1914, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1914, 0.62).
narrative_ontology:measurement_basis(turk_su_t1914, observed).
narrative_ontology:measurement(turk_su_t1919, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1919, 0.59).
narrative_ontology:measurement_basis(turk_su_t1919, observed).
narrative_ontology:measurement(turk_su_t1924, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1924, 0.51).
narrative_ontology:measurement_basis(turk_su_t1924, observed).
narrative_ontology:measurement(turk_su_t1928, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1928, 0.33).
narrative_ontology:measurement_basis(turk_su_t1928, observed).
narrative_ontology:measurement(turk_su_t1933, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1933, 0.16).
narrative_ontology:measurement_basis(turk_su_t1933, observed).
narrative_ontology:measurement(turk_su_t1938, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1938, 0.07).
narrative_ontology:measurement_basis(turk_su_t1938, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__ottoman_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__gradual_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Turkish script question' decomposes into three structurally distinct constraints — one per reading of the turkish_graphemic_substrate kernel. This file authors epsilon for the Arabic-script maintenance arrangement as the continuity reading assesses it (mature-phase extraction from the literacy barrier, offset by corpus-coordination value); the secular_nationalist_reading authors epsilon for the Latin-imposition arrangement (including its own transition coerctions); the gradual_transition_reading authors epsilon for the dual-script interim. The upstream member by empirical confidence is the secular nationalist reading (it governed after 1928 and its outcomes are directly observable); this reading influences the gradual reading's design space and forecloses the secular reading's identity premise. Epsilon differs across members because the arrangements differ, not because one constraint is measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
