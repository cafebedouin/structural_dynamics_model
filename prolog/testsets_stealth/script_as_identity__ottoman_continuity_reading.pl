% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__ottoman_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Ottoman Continuity Reading: Arabic Script as Constitutive of Turkish-Islamic Identity
 *   domain: linguistic/political/religious
 *
 * SUMMARY:
 *   Within the script_as_identity kernel, this story instantiates the
 *   ottoman_continuity_reading: the claim that Arabic script is constitutive
 *   of Turkish-Islamic identity and historical continuity, and the
 *   arrangement that maintained it as the empire's sole official medium from
 *   the Tanzimat era until the 1928 republican alphabet law. The arrangement
 *   coordinated a multilingual polity around one writable medium and gave
 *   believers continuous access to the Qur'anic corpus and Ottoman
 *   institutional memory; it also charged every learner years of acquisition
 *   labor in a script mismatched to Turkish vowel harmony, reserved fluent
 *   readership to a clerical-scribal caste, and answered rival alphabets with
 *   bans and exile. The colloquial label 'the script question' decomposes
 *   into three structurally distinct constraints — this continuity reading,
 *   the kemalist_rupture_reading, and the phonetic_instrumentalism_reading —
 *   linked as a constraint family; each has its own beneficiaries, victims,
 *   and stable epsilon. Claim and metrics are independent: the reading is
 *   CLAIMED as tangled_rope because both a genuine coordination function and
 *   asymmetric extraction are structurally present; the scalars describe the
 *   arrangement's operative-dominance phase (1862-1928), and the measurement
 *   series traces its rise, enforced peak, catastrophic 1928 reversal, and
 *   residual persistence.
 *
 * KEY AGENTS:
 *   - ulema_religious_authority: primary beneficiary and co-administrator (institutional/identity_locked) — collects interpretive authority; script mastery is its credential
 *   - ottoman_scribal_establishment: agenda_setter (institutional/constrained) — administers the chancery, controls professional entry, collects document-service fees
 *   - divan_calligraphic_elite: secondary beneficiary (organized/identity_locked) — prestige and patronage bound to the script's sacred aesthetics
 *   - turkish_peasant_majority: primary target (powerless/trapped) — bears the multi-year literacy cost; signs with marks
 *   - non_muslim_millet_communities: target at the interface (moderate/constrained) — pays interpreter costs at every paper boundary
 *   - provincial_latinizer_movements: targeted dissenters (moderate/trapped) — banned presses, exile publication
 *   - modernizing_officer_intellectuals: excluded powerful seat (powerful/mobile) — blocked reformers who ultimately captured the state and flipped the arrangement
 *   - comparative_script_historiography: analytical observer (analytical/analytical) — measures literacy, edits the archive, sees both sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.58).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.72).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Ottoman Continuity Reading: Arabic Script as Constitutive of Turkish-Islamic Identity").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "linguistic/political/religious").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, 'fbdd89a5-ed6e-43a8-a34c-4f58956a6e20').
narrative_ontology:cs_kernel_codification('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', fixed_text).
narrative_ontology:cs_authority_grounding('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', lineage).
narrative_ontology:cs_interpretation_layer_present('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20').
narrative_ontology:cs_reading_relation('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', script_as_identity__phonetic_instrumentalism_reading, forecloses).
narrative_ontology:cs_axiom('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', foundational, script_constitutes_community_identity).
narrative_ontology:cs_axiom_status(script_constitutes_community_identity, holdable).
narrative_ontology:cs_axiom_grounding('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', script_constitutes_community_identity, deontological).
narrative_ontology:cs_axiom('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', foundational, sacred_corpus_requires_original_script).
narrative_ontology:cs_axiom_status(sacred_corpus_requires_original_script, holdable).
narrative_ontology:cs_axiom_grounding('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', sacred_corpus_requires_original_script, theological).
narrative_ontology:cs_reference_frame('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', revelatory_script_continuity).
narrative_ontology:cs_drift_state('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', post_1928_republican_alphabet_law, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('fbdd89a5-ed6e-43a8-a34c-4f58956a6e20', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ulema_religious_authority).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_scribal_establishment).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, divan_calligraphic_elite).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, turkish_peasant_majority).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_muslim_millet_communities).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, provincial_latinizer_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the medreses, issue legal opinions, and certify teachers through license chains running back centuries; every link in those chains is written in Arabic script, and their standing rests on being the community's fluent readers of the Qur'an and the law books. When alphabet change was proposed they ruled on it as a religious question. Leaving the arrangement would mean renouncing the medium in which their own credentials, libraries, and daily practice are stored.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ulema_religious_authority, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ulema_religious_authority, agenda_setter).

% Run the chancery: land registers, tax rolls, court records, and foreign correspondence, all in the official script with its specialized numerical notation. Entry to the profession passes through apprenticeship in that script, which keeps the corps small, salaried, and indispensable; petitioners must pay scribes to write and read for them. Careers, fees, and archives are all denominated in the script the corps administers.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_scribal_establishment, agenda_setter,
    institutional, biographical, constrained, continental).

% Poets, master calligraphers, gilders, and manuscript painters whose reputations and patronage flow from the script's aesthetic and sacred register; their works are signed in it, priced in it, and transmitted through master-apprentice lineages in it.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, divan_calligraphic_elite, beneficiary,
    organized, generational, identity_locked, regional).

% Farm and soldier for a state whose records, taxes, and courts run in a script that takes years to learn and renders Turkish vowels ambiguously; most never acquire it, sign with marks, and depend on hired scribes for any dealing with paper. Schooling beyond a few years was scarce, and the script's difficulty made each additional year of literacy expensive.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, turkish_peasant_majority, payer,
    powerless, immediate, trapped, regional).

% Armenian, Greek, and Jewish communities run their own schools and presses in their own alphabets while the state's paperwork, courts, and commercial instruments operate in the official script; they pay interpreter and dragoman costs at every interface and face periodic pressure toward linguistic uniformity, especially in wartime.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, non_muslim_millet_communities, payer,
    moderate, generational, constrained, regional).

% Albanian and Kurdish writers who standardized Latin alphabets for their languages — the Manastir congress of 1908, the Istanbul and Geneva journals — and saw their newspapers banned, presses seized, and editors exiled in the 1910s; several continued publishing from Cairo, Sofia, and Geneva.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, provincial_latinizer_movements, payer,
    moderate, biographical, trapped, regional).

% Military officers, physicians, economists, and journalists who argued from the 1860s onward that the official script blocks science teaching, slows mass schooling, and cuts the country off from European print. Their proposals were declined by the chancery and ruled on by the religious establishment; some published abroad while holding careers inside the system. They remained outside the script regime's decision circle until capturing the new republic in 1923 and imposing the rival arrangement by law in 1928.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, modernizing_officer_intellectuals, excluded,
    powerful, generational, mobile, continental).

% Philologists, archivists, and historians inside and outside Turkey who measure literacy, catalog the press record, and edit the archival corpus; they read both sides of the reform debate and can price what each side's claims cost and delivered.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, comparative_script_historiography, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, ulema_religious_authority).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single script gives a multilingual, three-continent polity one writable medium for law, finance, correspondence, and scripture, and gives Turkish Muslims continuous access to the Qur'anic corpus and eleven centuries of Islamic textual production; entry into administration, commerce, and religious life runs through mastering it.
% TRANSFER_FUNCTION: Moves years of acquisition labor onto every learner in a script mismatched to Turkish vowel harmony; moves interpretive authority over texts to the clerical-scribal caste that alone reads them fluently; moves document-service fees to that caste, since the public cannot self-serve its own paperwork; and moves deference from lay readers to credentialed intermediaries.
% ABSENT_VOICES: The illiterate mass itself — the seat bearing the largest cost — had no voice; village women doubly so. Minority millet schools negotiated around the regime rather than within it. Provincial Latinizers were removed from the conversation by press bans and exile. Reform-minded officers and journalists submitted proposals that the chancery and the religious establishment declined to answer on the merits.
% DISAPPEARANCE_RATIONALE: It did rearrange: when the state adopted Latin letters in 1928, the scribal profession dissolved within a generation, medrese authority lost its monopoly on the written word, the Ottoman archive became a specialist language, and religious authority reorganized around oral transmission and new print forms. The arrangement's former holders also rearranged — Qur'an courses, Ottoman-script newspapers, calligraphy revivals — showing the arrangement depended on carriers rather than on nature.
% FOUNDING_PROBLEM: Binding a multilingual Muslim polity to its revealed corpus and administering an empire's paperwork demanded one authoritative script; Arabic script arrived as the vehicle of revelation and stayed as the vehicle of rule.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: social historians of the Tanzimat and Second Constitutional periods corroborate that the administrative founding problem was real and is now dead. Post-imperial Muslim communities outside the Ottoman establishment — Balkan diaspora congregations, Anatolian Qur'an-course networks — corroborate in practice that the identity-continuity problem is still treated as live by its holders. No source outside the beneficiary set attests that the administrative problem survives.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scalars characterize the arrangement during its operative dominance and sit just under the 1928 measurement row: extractiveness 0.58 reflects extraction that is heavy but bounded by a real coordination good — one writable medium for law, finance, scripture, and memory across three continents; suppression 0.72 reflects the enforcement actually deployed (print hesitancy, medrese gatekeeping, chancery monopoly, and the wartime bans on Albanian and Kurdish Latin alphabets); theater_ratio 0.22 reflects a medium doing real work everywhere, with ceremony a minor share. Accessibility_collapse 0.66: within the committed community the identity premise closes alternatives almost completely — adopting Latin letters becomes apostasy-adjacent — while outside it, reform proposals, minority alphabets, and foreign print kept alternatives visible. Resistance 0.56: sixty years of reform proposals, minority Latinization movements, and finally a victorious state-level reversal. The measurement series runs on one shared seven-point grid, every tracked metric authored at every point. Base_extractiveness peaks at 1928 (0.61) when the arrangement's cost stands fully exposed against the modernization program, then falls as its coercive scope collapses. Suppression_requirement traces the enforcement arc the story is centrally about: build-up to the 1928 peak (0.76), cliff-drop once the state flipped sides (0.21 by 1950), low-level persistence thereafter. Theater_ratio climbs monotonically after 1928 as the residue shifts from communication to commemoration and devotion. The suppression mechanism is mixed structural/internalized; the post-1928 persistence without enforcement is routed through the internalized_vs_structural_suppression omega rather than by inflating the scalar.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the ulema seat the arrangement is continuity itself: licenses, libraries, and daily practice are stored in the script, and the identity lock is professional-religious fusion — if the frame broke, their standing would convert into antiquarianism, so exit is unthinkable from inside. From the scribal seat it is a livelihood and a moat: administered orthography keeps the corps small and indispensable. From the peasant seat it is a wall: years of acquisition for ambiguous vowels, signing with marks, paying scribes to address one's own government. The powerful excluded seat experiences blockage with exit — censorship at home, journals abroad — and its mobility is precisely what let it capture the state in 1923-28 and impose the rival reading. Same-polity actors diverge because the arrangement differentiates exit: identity locks the clergy, closure binds the scribes, poverty traps the peasantry, and foreign credentials mobilize the officers. The observer seat prices all of this from outside the bargain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: ulema, scribal establishment, and calligraphic elite sit near the beneficiary end, with identity_lock pinning the ulema and calligraphers hardest — their d is damped furthest because exit would dissolve the self that exits. Victim declarations drive the opposite pole: peasants (trapped, powerless) approach full target; millet communities and provincial Latinizers sit high-d with constrained or no exit. One override is authored: for the powerful atom. The modernizing officer-intellectual seat is structurally a target — its programs were blocked, its journals censored — but holds arbitrage-grade exit, so the canonical powerful fallback (near-symmetric) would understate its targeting; d is overridden to 0.6, letting mobile exit damp effective extraction while recording the true directional position. Effective extraction concentrates where power is weakest and exit narrowest: continental scope amplifies verification costs, and the burden lands on exactly the seats least able to verify or refuse it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the arrangement as pure extraction erases the real coordination good — a single medium binding a multilingual polity to its law, commerce, and revealed corpus; reading it as pure coordination erases the literacy rents, the closed scribal corps, and the banned rivals. Tangled_rope holds both halves, which is why the structural triple (active enforcement, beneficiaries, victims) is authored. The mandate then splits historically: the administrative mandate died with the empire, while the identity mandate persists by its holders' lights — hence founding_problem_status is contested rather than dead, avoiding the dead-plus-world_rearranges zombie signature without flattery. The measurement series lets the engine date the transition instead of averaging it away: extraction and enforcement peak together in 1928 and collapse after, while theater climbs — the residue drifts piton-ward (commemorative calligraphy, nostalgia print) without this story asserting the drift complete. Mandatrophy here is real but partial: the arrangement outlived its administrative function and retained a voluntary devotional one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_of_script,
    'This constraint is the ottoman_continuity_reading of the script_as_identity kernel: is the Arabic script''s relation to Turkish-Islamic identity constitutive (this reading), severable for modernization (kemalist_rupture_reading), or instrumentally neutral with Latin superior on phonetic fit (phonetic_instrumentalism_reading) — and which structural elements (beneficiary set, victim set, epsilon) shift under each sibling?',
    'Not resolvable by data within this story: the siblings are separate constraint files. Resolution proceeds by comparing the family''s computed classifications and by asking which premise each party could abandon without dissolving its own position.',
    'Under the rupture reading the victim set expands to the whole modernizing population and epsilon rises; under the instrumentalist reading the arrangement collapses to a reversible technical choice with near-zero identity extraction; under this reading the arrangement is continuity itself and measured extraction risks misreading sacred obligation as rent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status_of_script, conceptual, 'Committer structure: one reading of the script_as_identity kernel; sibling readings would restructure beneficiaries, victims, and epsilon.').

omega_variable(
    identity_constitution_vs_construction,
    'Is the script-identity bond a deep feature of Turkish-Islamic civilization, or a contingent arrangement whose identity-constitution was produced by the authorities who benefited from script-gated literacy?',
    'Comparative Turkic-Muslim cases that changed scripts while retaining identity: Volga Tatar alphabet reforms, Yugoslav Muslim use of Latin-script Serbo-Croatian, Uyghur script transitions. Survival of identity across script change indicates contingency.',
    'If contingent, the arrangement is a constructed coordination-plus-extraction structure with identifiable beneficiaries; if constitutive, part of the measured extraction is mispriced obligation and the beneficiary framing weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_constitution_vs_construction, empirical, 'Whether the identity-constitution of the script is native to the community or manufactured by its gatekeepers.').

omega_variable(
    internalized_vs_structural_suppression,
    'Was the suppression holding the arrangement structural (press bans, medrese gatekeeping, chancery monopoly) or internalized (believers experienced Latin adoption as betrayal, requiring little enforcement)?',
    'Post-1928 natural experiment: state enforcement fell to near zero while Ottoman-script devotion persisted for decades in Qur''an courses, conservative newspapers, and diaspora communities with no enforcement at all; persistence without enforcement indicates a substantial internalized component.',
    'If largely internalized, the pre-1928 suppression requirement overstated the coercion needed at the margin and understates the identity-lock; the residue''s persistence curve is carried by belief rather than force, softening the enforcement-based reading of the post-reform period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized suppression mechanism behind script adherence.').

omega_variable(
    script_vs_resource_literacy_attribution,
    'What share of mass illiteracy under the arrangement is attributable to the script''s acquisition difficulty rather than to school scarcity, child labor, and poverty?',
    'Compare literacy trajectories across same-script-family contexts (Persian, Urdu) at different resource levels, and isolate the 1928-29 literacy movement attributable to the alphabet change in Republican census and night-school records.',
    'If the script''s causal share is small, extraction attributed to the arrangement contracts toward ordinary governance cost and the structure tilts toward pure coordination; if large, the peasant seat''s directionality approaches full target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(script_vs_resource_literacy_attribution, empirical, 'Attribution of the literacy burden between script difficulty and resource scarcity.').

omega_variable(
    archive_access_transliteration_substitutability,
    'Does maintaining Arabic-script literacy genuinely preserve access to Ottoman institutional memory, or can transliteration and critical editions substitute at acceptable loss?',
    'Throughput audit of the Ottoman Archives'' transliteration and cataloging programs against uncatalogued holdings; comparative productivity of script-literate versus transliteration-dependent researchers on identical corpora.',
    'If substitutable, the continuity warrant shrinks toward authority preservation and the coordination function thins; if not, the arrangement carries a genuine coordination good that the rival readings underprice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_access_transliteration_substitutability, empirical, 'Substitutability of transliteration for script literacy in accessing the Ottoman archive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1862, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1862, script_as_identity__ottoman_continuity_reading, theater_ratio, 1862, 0.14).
narrative_ontology:measurement_basis(scri_tr_t1862, observed).
narrative_ontology:measurement(scri_tr_t1908, script_as_identity__ottoman_continuity_reading, theater_ratio, 1908, 0.17).
narrative_ontology:measurement_basis(scri_tr_t1908, observed).
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__ottoman_continuity_reading, theater_ratio, 1928, 0.24).
narrative_ontology:measurement_basis(scri_tr_t1928, observed).
narrative_ontology:measurement(scri_tr_t1950, script_as_identity__ottoman_continuity_reading, theater_ratio, 1950, 0.41).
narrative_ontology:measurement_basis(scri_tr_t1950, observed).
narrative_ontology:measurement(scri_tr_t1980, script_as_identity__ottoman_continuity_reading, theater_ratio, 1980, 0.49).
narrative_ontology:measurement_basis(scri_tr_t1980, observed).
narrative_ontology:measurement(scri_tr_t2000, script_as_identity__ottoman_continuity_reading, theater_ratio, 2000, 0.54).
narrative_ontology:measurement_basis(scri_tr_t2000, observed).
narrative_ontology:measurement(scri_tr_t2024, script_as_identity__ottoman_continuity_reading, theater_ratio, 2024, 0.59).
narrative_ontology:measurement_basis(scri_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t1862, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1862, 0.46).
narrative_ontology:measurement_basis(scri_be_t1862, observed).
narrative_ontology:measurement(scri_be_t1908, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1908, 0.53).
narrative_ontology:measurement_basis(scri_be_t1908, observed).
narrative_ontology:measurement(scri_be_t1928, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1928, 0.61).
narrative_ontology:measurement_basis(scri_be_t1928, observed).
narrative_ontology:measurement(scri_be_t1950, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement_basis(scri_be_t1950, observed).
narrative_ontology:measurement(scri_be_t1980, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement_basis(scri_be_t1980, observed).
narrative_ontology:measurement(scri_be_t2000, script_as_identity__ottoman_continuity_reading, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement_basis(scri_be_t2000, observed).
narrative_ontology:measurement(scri_be_t2024, script_as_identity__ottoman_continuity_reading, base_extractiveness, 2024, 0.26).
narrative_ontology:measurement_basis(scri_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1862, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1862, 0.44).
narrative_ontology:measurement_basis(scri_su_t1862, observed).
narrative_ontology:measurement(scri_su_t1908, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1908, 0.61).
narrative_ontology:measurement_basis(scri_su_t1908, observed).
narrative_ontology:measurement(scri_su_t1928, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1928, 0.76).
narrative_ontology:measurement_basis(scri_su_t1928, observed).
narrative_ontology:measurement(scri_su_t1950, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1950, 0.21).
narrative_ontology:measurement_basis(scri_su_t1950, observed).
narrative_ontology:measurement(scri_su_t1980, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1980, 0.16).
narrative_ontology:measurement_basis(scri_su_t1980, observed).
narrative_ontology:measurement(scri_su_t2000, script_as_identity__ottoman_continuity_reading, suppression_requirement, 2000, 0.13).
narrative_ontology:measurement_basis(scri_su_t2000, observed).
narrative_ontology:measurement(scri_su_t2024, script_as_identity__ottoman_continuity_reading, suppression_requirement, 2024, 0.1).
narrative_ontology:measurement_basis(scri_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1862, tn=2024
narrative_ontology:measurement(scri_grid_01, script_as_identity__ottoman_continuity_reading, accessibility_collapse(class), 1862, 0.5).
narrative_ontology:measurement_basis(scri_grid_01, observed).
narrative_ontology:measurement(scri_grid_02, script_as_identity__ottoman_continuity_reading, accessibility_collapse(class), 2024, 0.3).
narrative_ontology:measurement_basis(scri_grid_02, observed).
narrative_ontology:measurement(scri_grid_03, script_as_identity__ottoman_continuity_reading, accessibility_collapse(individual), 1862, 0.7).
narrative_ontology:measurement_basis(scri_grid_03, observed).
narrative_ontology:measurement(scri_grid_04, script_as_identity__ottoman_continuity_reading, accessibility_collapse(individual), 2024, 0.3).
narrative_ontology:measurement_basis(scri_grid_04, observed).
narrative_ontology:measurement(scri_grid_05, script_as_identity__ottoman_continuity_reading, accessibility_collapse(organizational), 1862, 0.75).
narrative_ontology:measurement_basis(scri_grid_05, observed).
narrative_ontology:measurement(scri_grid_06, script_as_identity__ottoman_continuity_reading, accessibility_collapse(organizational), 2024, 0.35).
narrative_ontology:measurement_basis(scri_grid_06, observed).
narrative_ontology:measurement(scri_grid_07, script_as_identity__ottoman_continuity_reading, accessibility_collapse(structural), 1862, 0.8).
narrative_ontology:measurement_basis(scri_grid_07, observed).
narrative_ontology:measurement(scri_grid_08, script_as_identity__ottoman_continuity_reading, accessibility_collapse(structural), 2024, 0.4).
narrative_ontology:measurement_basis(scri_grid_08, observed).
narrative_ontology:measurement(scri_grid_09, script_as_identity__ottoman_continuity_reading, resistance(class), 1862, 0.5).
narrative_ontology:measurement_basis(scri_grid_09, observed).
narrative_ontology:measurement(scri_grid_10, script_as_identity__ottoman_continuity_reading, resistance(class), 2024, 0.05).
narrative_ontology:measurement_basis(scri_grid_10, observed).
narrative_ontology:measurement(scri_grid_11, script_as_identity__ottoman_continuity_reading, resistance(individual), 1862, 0.2).
narrative_ontology:measurement_basis(scri_grid_11, observed).
narrative_ontology:measurement(scri_grid_12, script_as_identity__ottoman_continuity_reading, resistance(individual), 2024, 0.1).
narrative_ontology:measurement_basis(scri_grid_12, observed).
narrative_ontology:measurement(scri_grid_13, script_as_identity__ottoman_continuity_reading, resistance(organizational), 1862, 0.25).
narrative_ontology:measurement_basis(scri_grid_13, observed).
narrative_ontology:measurement(scri_grid_14, script_as_identity__ottoman_continuity_reading, resistance(organizational), 2024, 0.1).
narrative_ontology:measurement_basis(scri_grid_14, observed).
narrative_ontology:measurement(scri_grid_15, script_as_identity__ottoman_continuity_reading, resistance(structural), 1862, 0.3).
narrative_ontology:measurement_basis(scri_grid_15, observed).
narrative_ontology:measurement(scri_grid_16, script_as_identity__ottoman_continuity_reading, resistance(structural), 2024, 0.1).
narrative_ontology:measurement_basis(scri_grid_16, observed).
narrative_ontology:measurement(scri_grid_17, script_as_identity__ottoman_continuity_reading, stakes_inflation(class), 1862, 0.4).
narrative_ontology:measurement_basis(scri_grid_17, observed).
narrative_ontology:measurement(scri_grid_18, script_as_identity__ottoman_continuity_reading, stakes_inflation(class), 2024, 0.15).
narrative_ontology:measurement_basis(scri_grid_18, observed).
narrative_ontology:measurement(scri_grid_19, script_as_identity__ottoman_continuity_reading, stakes_inflation(individual), 1862, 0.5).
narrative_ontology:measurement_basis(scri_grid_19, observed).
narrative_ontology:measurement(scri_grid_20, script_as_identity__ottoman_continuity_reading, stakes_inflation(individual), 2024, 0.2).
narrative_ontology:measurement_basis(scri_grid_20, observed).
narrative_ontology:measurement(scri_grid_21, script_as_identity__ottoman_continuity_reading, stakes_inflation(organizational), 1862, 0.6).
narrative_ontology:measurement_basis(scri_grid_21, observed).
narrative_ontology:measurement(scri_grid_22, script_as_identity__ottoman_continuity_reading, stakes_inflation(organizational), 2024, 0.25).
narrative_ontology:measurement_basis(scri_grid_22, observed).
narrative_ontology:measurement(scri_grid_23, script_as_identity__ottoman_continuity_reading, stakes_inflation(structural), 1862, 0.55).
narrative_ontology:measurement_basis(scri_grid_23, observed).
narrative_ontology:measurement(scri_grid_24, script_as_identity__ottoman_continuity_reading, stakes_inflation(structural), 2024, 0.2).
narrative_ontology:measurement_basis(scri_grid_24, observed).
narrative_ontology:measurement(scri_grid_25, script_as_identity__ottoman_continuity_reading, suppression(class), 1862, 0.45).
narrative_ontology:measurement_basis(scri_grid_25, observed).
narrative_ontology:measurement(scri_grid_26, script_as_identity__ottoman_continuity_reading, suppression(class), 2024, 0.05).
narrative_ontology:measurement_basis(scri_grid_26, observed).
narrative_ontology:measurement(scri_grid_27, script_as_identity__ottoman_continuity_reading, suppression(individual), 1862, 0.4).
narrative_ontology:measurement_basis(scri_grid_27, observed).
narrative_ontology:measurement(scri_grid_28, script_as_identity__ottoman_continuity_reading, suppression(individual), 2024, 0.15).
narrative_ontology:measurement_basis(scri_grid_28, observed).
narrative_ontology:measurement(scri_grid_29, script_as_identity__ottoman_continuity_reading, suppression(organizational), 1862, 0.55).
narrative_ontology:measurement_basis(scri_grid_29, observed).
narrative_ontology:measurement(scri_grid_30, script_as_identity__ottoman_continuity_reading, suppression(organizational), 2024, 0.1).
narrative_ontology:measurement_basis(scri_grid_30, observed).
narrative_ontology:measurement(scri_grid_31, script_as_identity__ottoman_continuity_reading, suppression(structural), 1862, 0.5).
narrative_ontology:measurement_basis(scri_grid_31, observed).
narrative_ontology:measurement(scri_grid_32, script_as_identity__ottoman_continuity_reading, suppression(structural), 2024, 0.05).
narrative_ontology:measurement_basis(scri_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, information_standard).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% The colloquial 'script question' decomposes into three epsilon-distinct constraints sharing the script_as_identity kernel: this continuity reading (coordination plus extraction, enforced, reversed in 1928), the kemalist_rupture_reading (the successor arrangement's warrant), and the phonetic_instrumentalism_reading (the empirical substrate all parties concede — Latin's phonetic fit — which functions upstream: even continuity defenders concede the efficiency loss and argue identity outweighs it). Family links run through affects_constraints; each member keeps its own beneficiaries, victims, and stable epsilon per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
