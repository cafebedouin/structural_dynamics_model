% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   domain: religious/philosophical/cultural
 *
 * SUMMARY:
 *   This story instantiates the incoherent_bundle reading of the kami-buddha
 *   ontology kernel: shinbutsu-shugo is not one settled ontological claim but
 *   a historically accreted, institutionally profitable bundle of mutually
 *   incompatible commitments (kami as unsalvaged spirits needing Buddhist
 *   liberation; kami as honji-suijaku traces identical in essence to buddhas;
 *   kami and buddhas as functionally separate domains) that different sites,
 *   lineages, and periods invoke opportunistically. The bundle persists not
 *   because any one framework is true but because shrine-temple complexes and
 *   ritual specialist lineages extract ongoing institutional value from never
 *   having to choose. Systematizing projects (Ryobo Shinto, Yoshida Shinto,
 *   the Meiji separation edicts) repeatedly attempt to force coherence and
 *   repeatedly fail to fully displace the underlying practical syncretism.
 *   This is a sibling story to honji_suijaku_monism (which reads the kernel
 *   as a genuine, coherent identity claim) and domain_partition (which reads
 *   it as a genuine, coherent functional division) — this reading instead
 *   denies that either coherent reading was ever the operative ontology on
 *   the ground.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: institutional beneficiary of preserved ambiguity
 *   - ritual_specialist_lineages: professional beneficiaries whose expertise requires the contradiction
 *   - systematic_theologians: pay in absorbed or ignored systematizing labor
 *   - lay_practitioners_seeking_doctrinal_clarity: pay in irreducible confusion, cannot exit
 *   - meiji_state_separation_reformers: excluded reformist voice whose forced resolution was partially rolled back
 *   - comparative_religion_scholars: analytical observers documenting the millennium-long pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.52).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.44).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, piton).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious/philosophical/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, 'd5e64929-4ce5-4bbc-9131-d099c4e6f777').
narrative_ontology:cs_kernel_codification('d5e64929-4ce5-4bbc-9131-d099c4e6f777', distributed).
narrative_ontology:cs_authority_grounding('d5e64929-4ce5-4bbc-9131-d099c4e6f777', practice).
narrative_ontology:cs_interpretation_layer_present('d5e64929-4ce5-4bbc-9131-d099c4e6f777').
narrative_ontology:cs_reading_relation('d5e64929-4ce5-4bbc-9131-d099c4e6f777', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('d5e64929-4ce5-4bbc-9131-d099c4e6f777', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('d5e64929-4ce5-4bbc-9131-d099c4e6f777', foundational, no_single_operative_ontology_ever_governed_practice).
narrative_ontology:cs_axiom_status(no_single_operative_ontology_ever_governed_practice, holdable).
narrative_ontology:cs_axiom_grounding('d5e64929-4ce5-4bbc-9131-d099c4e6f777', no_single_operative_ontology_ever_governed_practice, empirically_contingent).
narrative_ontology:cs_axiom('d5e64929-4ce5-4bbc-9131-d099c4e6f777', foundational, institutional_persistence_explains_survival_not_theological_truth).
narrative_ontology:cs_axiom_status(institutional_persistence_explains_survival_not_theological_truth, holdable).
narrative_ontology:cs_axiom_grounding('d5e64929-4ce5-4bbc-9131-d099c4e6f777', institutional_persistence_explains_survival_not_theological_truth, empirically_contingent).
narrative_ontology:cs_axiom('d5e64929-4ce5-4bbc-9131-d099c4e6f777', secondary, ritual_efficacy_substitutes_for_doctrinal_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_substitutes_for_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('d5e64929-4ce5-4bbc-9131-d099c4e6f777', ritual_efficacy_substitutes_for_doctrinal_coherence, instrumental).
narrative_ontology:cs_reference_frame('d5e64929-4ce5-4bbc-9131-d099c4e6f777', pre_systematized_accommodation_practice).
narrative_ontology:cs_drift_state('d5e64929-4ce5-4bbc-9131-d099c4e6f777', meiji_separation_edicts, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d5e64929-4ce5-4bbc-9131-d099c4e6f777', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, ritual_specialist_lineages).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, systematic_theologians).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_over_doctrinal_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jingu-ji and miyadera complexes administer combined kami-buddha ritual precincts, drawing revenue, land grants, and pilgrim traffic from BOTH the kami cult and the Buddhist institution simultaneously. They benefit precisely from never resolving whether the kami is a subordinate spirit needing salvation, an equal partner, or a local manifestation of a cosmic buddha — each framing justifies a different ritual service, a different donation stream, and a different claim on adjacent land and labor. Coherence would force a choice that shrinks their institutional footprint.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shrine_temple_administrative_complexes, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, shrine_temple_administrative_complexes, agenda_setter).

% Shugenja, shrine priests, and temple clergy hold hereditary or trained expertise in specific ritual sequences (kanjo, goma, kagura) that only make sense if the fusion/separation ambiguity is preserved — their professional standing depends on being the ones who can navigate the contradiction, not on resolving it. Their livelihoods and social identity are constituted through mediating a system that would not need mediators if it were doctrinally settled.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ritual_specialist_lineages, beneficiary,
    organized, generational, identity_locked, regional).

% Buddhist and Shinto scholars attempting to produce a consistent honji-suijaku theology, or a consistent separation theology, repeatedly run into ritual practices, textual traditions, and institutional arrangements that contradict whichever framework they propose. They pay in wasted intellectual labor and in having their systematizing projects (e.g. Ryobu Shinto, Yoshida Shinto) absorbed, distorted, or ignored by the practical apparatus rather than adopted as governing doctrine.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, systematic_theologians, payer,
    moderate, biographical, constrained, regional).

% Ordinary worshippers who ask whether praying to a kami is the same act as venerating a bodhisattva, or a separate act entirely, receive answers that shift by shrine, by season, by ritual occasion, and by which specialist they consult. They bear the confusion cost and cannot exit the ambiguity because the ambiguity is not localized in any one institution they could simply avoid — it is diffused across the entire religious landscape they must operate within.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% The 1868 shinbutsu bunri (separation) edicts attempted to impose a clean ontological partition by state fiat, forcibly disestablishing jingu-ji complexes and purging Buddhist elements from shrines. Their attempt to resolve the incoherence into domain_partition was itself absorbed and partially reversed by continuing syncretic practice at the popular level — the bundle proved more durable than the state's attempted resolution, which is why this reformist voice sits outside the surviving arrangement rather than having settled it.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, meiji_state_separation_reformers, excluded,
    powerful, generational, constrained, national).

% Historians and religious studies scholars document that shinbutsu-shugo functioned for roughly a millennium without ever resolving into a single coherent ontology, and that every systematizing school (Ryobu, Sanno, Yoshida) coexisted with unsystematized local practice rather than replacing it. They observe the pattern across sites and periods without holding institutional stake in any resolution.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a single ritual and institutional infrastructure to serve multiple, mutually incompatible theological needs simultaneously — purification concerns, salvific concerns, ancestor concerns, agricultural/fertility concerns — without forcing communities to build and maintain separate, competing institutions for each.
% TRANSFER_FUNCTION: Moves land grants, tax exemptions, pilgrim revenue, and lay donations toward whichever institutional framing (kami-as-subordinate, kami-as-honji-suijaku-trace, kami-as-separate-deity) is locally advantageous at a given moment, while moving the cost of unresolved doctrinal contradiction onto scholars and lay believers who want a consistent answer.
% ABSENT_VOICES: Lay practitioners who want to know 'what is actually true' are structurally unable to force a resolution because no single authority owns the whole system; the Meiji separationists tried to force one and were partially rolled back by continuing practice, showing the exclusion is not merely historical but persistent.
% DISAPPEARANCE_RATIONALE: If the incoherent bundle were somehow forced into full resolution (either total fusion or total separation), the surviving jingu-ji-style complexes, dual-function ritual specialists, and syncretic pilgrimage economies would have to reorganize substantially — some sites did exactly this after 1868 and permanently lost institutional functions. But popular ritual practice at many shrines shows the underlying religious behavior (praying for this-worldly benefit, honoring ancestors, seeking purification) persisting under whatever label survives, suggesting part of the world would rearrange and part would not; the parties dispute which part is essential.
% FOUNDING_PROBLEM: Early Japanese Buddhism needed to establish itself in a landscape already saturated with kami cults tied to specific land, lineage, and political legitimacy; some accommodation between the imported salvific system and the indigenous cultic system was necessary for Buddhism to take root without simply displacing existing authority structures.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars attest, from outside any shrine or temple's institutional interest, that the accommodation problem was real in the 8th-9th centuries but that by the medieval period the bundle had become self-perpetuating independent of any live accommodation need — shrine-temple complexes and ritual lineages themselves assert the founding problem remains live because it justifies present arrangements, but this corroboration comes entirely from the benefiting parties and should be weighted accordingly.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.52) rather than high because the bundle's cost to lay practitioners is diffuse confusion rather than acute material extraction — no one is charged a fee for the ambiguity itself, but institutional complexes do capture land, tax, and pilgrim revenue that a resolved, simplified system might not sustain at the same scale. Suppression is moderate (0.44): no single authority coercively enforces the incoherence, but the diffusion of authority across many sites and lineages makes local dissent structurally ineffective at forcing a resolution, which functions similarly to suppression without a single enforcer. Theater ratio starts low (0.2) when the accommodation was a live functional necessity in the 8th-9th centuries and rises substantially (0.68) by the time syncretic ritual complexes had become self-perpetuating institutional theater with only nominal live doctrinal work behind them — this is the piton signature: an original coordination function (accommodating a real theological and political problem) atrophying into performative maintenance. Accessibility collapse is moderate-low (0.4): alternative, more coherent framings were always available and periodically attempted (both fusion and separation theologies existed as live options throughout), which is precisely why this reading claims incoherence rather than settled monism or settled partition. Resistance is moderate-high (0.58): both theologians and reformist state actors have repeatedly and visibly pushed against the bundle, which is exactly what a genuinely incoherent, institutionally-sustained arrangement should provoke.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple complexes and ritual specialists sit near the beneficiary end: their institutional and professional standing is constituted BY the ambiguity, and they have durable arbitrage-grade or identity-locked positions that let them draw value from whichever framing suits a given occasion. Theologians and lay practitioners sit near the target end: they bear the cost of unresolved contradiction (wasted systematizing labor, irreducible confusion) without the option of exiting into a coherent alternative, because the ambiguity is diffused across the entire religious landscape rather than concentrated in one avoidable institution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (accommodating an imported salvific religion within an existing indigenous cultic landscape) was genuinely live in the 8th-9th centuries. By the medieval and early modern periods, that specific accommodation problem had largely been solved one way or another at most individual sites, but the institutional apparatus built to manage the accommodation persisted and diversified its revenue and legitimacy functions independent of the original problem. This is the piton pattern: the mismatch between founding_problem_status (contested, trending toward dead) and disappearance_verdict (contested, trending toward world_rearranges for institutions but world_unchanged for underlying lay religious behavior) signals exactly the capture/zombie flag the six-questions battery is designed to surface — corroboration for continued live-ness comes almost entirely from the benefiting institutional parties themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_illusion_vs_genuine_incoherence,
    'Is the appearance of incoherence itself a modern scholarly artifact of applying Western analytic standards of doctrinal consistency to a tradition that never valued or required propositional consistency in the first place, or was the incoherence experienced as a live tension by premodern practitioners and theologians themselves?',
    'Close reading of premodern commentarial and polemical literature (e.g. Yoshida Kanetomo''s writings, Buddhist responses to Ryobu Shinto) for explicit acknowledgment of tension versus untroubled fluidity between framings; compare against how much energy was actually invested in systematizing projects.',
    'If premodern actors experienced no tension, this reading overstates incoherence as extraction and the story is closer to a rope (workable, low-friction pluralism) than a piton; if tension was live and repeatedly suppressed or absorbed, the piton/extraction reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_illusion_vs_genuine_incoherence, conceptual, 'Whether incoherence was lived tension or is a retrospective analytic imposition.').

omega_variable(
    meiji_separation_as_resolution_or_new_bundle,
    'Did the 1868 shinbutsu bunri edicts actually resolve the incoherence into a clean domain_partition, or did they simply produce a new, differently incoherent bundle (State Shinto''s own internal contradictions between ''not a religion'' and de facto religious establishment)?',
    'Historical analysis of post-1868 shrine administration and State Shinto legal status through 1945, tracking whether kami-buddha entanglement persisted informally at the popular level despite formal separation.',
    'If the separation produced a genuinely different, coherent partition, this reading''s claim that ''separation attempts fail'' is weakened for that specific case; if it produced only a relabeled incoherence, the incoherent_bundle reading is strengthened as the more durable structural description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_resolution_or_new_bundle, empirical, 'Whether Meiji-era state separation genuinely resolved or merely relabeled the underlying incoherence.').

omega_variable(
    kernel_framing_alternative_reading_selection,
    'Given that the same historical record (jingu-ji complexes, honji-suijaku doctrine, domain-specific ritual practice, Meiji separation) can support all three sibling readings, what specific evidentiary or theoretical commitments led to selecting the incoherent_bundle framing over honji_suijaku_monism or domain_partition for this story?',
    'This reading was selected because it privileges institutional and practical-ritual evidence (site-by-site variation, coexistence of contradictory framings without resolution, repeated failure of systematizing projects) over doctrinal-textual evidence (which could support monism) or administrative-functional evidence (which could support partition taken from later, more bureaucratized periods). A monism-privileging reading would weight honji-suijaku textual systematization more heavily and treat site variation as surface diversity over a deeper unity; a partition-privileging reading would weight administrative and ritual-calendar function assignment more heavily.',
    'Adopting honji_suijaku_monism would classify this constraint as closer to mountain or rope (coherent theological achievement, low institutional extraction); adopting domain_partition would classify it as closer to rope (functional division of labor, moderate extraction); this reading''s claim of institutional extraction via unresolved contradiction produces a piton/tangled-rope-adjacent profile instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading_selection, conceptual, 'Documents the framing choice among three coherent alternative readings of the same historical record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 200, 0.3).
narrative_ontology:measurement_basis(kami_tr_t200, projected).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__incoherent_bundle, theater_ratio, 400, 0.42).
narrative_ontology:measurement_basis(kami_tr_t400, projected).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.5).
narrative_ontology:measurement_basis(kami_tr_t600, projected).
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__incoherent_bundle, theater_ratio, 800, 0.58).
narrative_ontology:measurement_basis(kami_tr_t800, projected).
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1000, 0.64).
narrative_ontology:measurement_basis(kami_tr_t1000, observed).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.68).
narrative_ontology:measurement_basis(kami_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 200, 0.4).
narrative_ontology:measurement_basis(kami_be_t200, projected).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 400, 0.45).
narrative_ontology:measurement_basis(kami_be_t400, projected).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.48).
narrative_ontology:measurement_basis(kami_be_t600, projected).
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 800, 0.5).
narrative_ontology:measurement_basis(kami_be_t800, projected).
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1000, 0.51).
narrative_ontology:measurement_basis(kami_be_t1000, observed).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.52).
narrative_ontology:measurement_basis(kami_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__incoherent_bundle, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member family reading the kami_buddha_ontology kernel: honji_suijaku_monism (coherent identity claim), domain_partition (coherent functional division), and this incoherent_bundle reading (denial that either coherent claim was ever the operative ontology). Each story carries its own stable epsilon and classification; they are linked here rather than merged because they instantiate structurally distinct claims about the same historical phenomenon, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
