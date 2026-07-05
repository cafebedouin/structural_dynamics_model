% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: 1928 Turkish Script Reform as Deliberate Civilizational Rupture
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the RUPTURE reading of the orthographic_kernel:
 *   the 1928 Turkish alphabet reform understood not primarily as technical
 *   modernization (that is a sibling constraint, modernization_reading) nor
 *   as an act preserving continuity (continuity_reading — the reading this
 *   one directly opposes), but as a deliberately engineered civilizational
 *   break designed to sever the population's living access to Ottoman-Islamic
 *   textual, legal, and religious tradition and to relocate cultural and
 *   interpretive authority in a new secular state apparatus. Under this
 *   reading, ε is very high because the victim set is essentially the entire
 *   pre-reform literate population plus the institutional carriers of
 *   Ottoman-Islamic textual tradition (the ulema, manuscript culture), while
 *   the beneficiary set is the new state apparatus and the cohort it
 *   produces. This is a different structural claim from
 *   modernization_reading, which would locate roughly the same historical
 *   event's ε much lower by treating literacy-cost reduction and technical
 *   suitability as the operative function with continuity preserved through
 *   Ottoman Turkish vocabulary — a different observable of the same event
 *   yielding a different ε, which is why these are two separate constraint
 *   files rather than one story with a measurement parameter.
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: Primary agenda-setter (institutional/arbitrage) — designs and enforces total, rapid transition
 *   - pre_reform_literate_population: Primary target (powerless/trapped) — loses functional literacy overnight
 *   - ottoman_ulema_class: Secondary target (organized/trapped) — loses interpretive monopoly as part of broader disestablishment
 *   - new_national_literacy_elite: Secondary beneficiary (powerful/mobile) — inherits gatekeeping position
 *   - linguistic_and_historical_analysts: Analytical observer — assesses necessity of rupture vs. gradualism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.87).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.91).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "1928 Turkish Script Reform as Deliberate Civilizational Rupture").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '3e5d728f-7f65-416a-a2c2-938ff43d7bde').
narrative_ontology:cs_kernel_codification('3e5d728f-7f65-416a-a2c2-938ff43d7bde', formalized).
narrative_ontology:cs_authority_grounding('3e5d728f-7f65-416a-a2c2-938ff43d7bde', extraction).
narrative_ontology:cs_interpretation_layer_present('3e5d728f-7f65-416a-a2c2-938ff43d7bde').
narrative_ontology:cs_reading_relation('3e5d728f-7f65-416a-a2c2-938ff43d7bde', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('3e5d728f-7f65-416a-a2c2-938ff43d7bde', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('3e5d728f-7f65-416a-a2c2-938ff43d7bde', foundational, civilizational_break_is_legitimate_state_founding_act).
narrative_ontology:cs_axiom_status(civilizational_break_is_legitimate_state_founding_act, holdable).
narrative_ontology:cs_axiom_grounding('3e5d728f-7f65-416a-a2c2-938ff43d7bde', civilizational_break_is_legitimate_state_founding_act, conventional).
narrative_ontology:cs_axiom('3e5d728f-7f65-416a-a2c2-938ff43d7bde', foundational, ottoman_islamic_textual_authority_must_be_delegitimized_for_secular_sovereignty).
narrative_ontology:cs_axiom_status(ottoman_islamic_textual_authority_must_be_delegitimized_for_secular_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3e5d728f-7f65-416a-a2c2-938ff43d7bde', ottoman_islamic_textual_authority_must_be_delegitimized_for_secular_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('3e5d728f-7f65-416a-a2c2-938ff43d7bde', ottoman_islamic_textual_continuity_order).
narrative_ontology:cs_drift_state('3e5d728f-7f65-416a-a2c2-938ff43d7bde', post_1928_republican_consolidation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3e5d728f-7f65-416a-a2c2-938ff43d7bde', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_educational_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, new_national_literacy_elite).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_ulema_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, religious_manuscript_tradition_bearers).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, elderly_and_rural_populations).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, secular_national_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, civilizational_westward_reorientation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates the Latin alphabet as mandatory within months (Law No. 1353, November 1928), establishes Millet Mektepleri (Nation's Schools) to compel adult re-literacy, and criminalizes continued public use of Arabic script in official contexts. Frames the change as necessary modernization but structures it to sever legal, religious, and administrative continuity with the Ottoman and Islamic textual past in a single stroke. Captures the legitimacy dividend of appearing to have created a wholly new nation.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains a permanent institutional mandate and career pipeline built entirely on the new orthography: textbook production, teacher training, literacy campaigns. Its authority and continued funding depend on the rupture being framed as necessary and irreversible rather than as one contestable choice among others.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_educational_bureaucracy, beneficiary,
    institutional, generational, arbitrage, national).

% Young, urban, and closely tied to the new republic acquire fluency in the new script quickly and inherit gatekeeping positions in publishing, civil service, and education that the older literate class cannot easily contest, since the older class's credential — Arabic-script literacy — has been declared obsolete overnight.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, new_national_literacy_elite, beneficiary,
    powerful, biographical, mobile, national).

% Millions who had achieved literacy in Arabic script become functionally illiterate in their own country within a single generation. Cannot read new official documents, newspapers, or their children's schoolbooks. No transitional grace period was built in; the change was designed to be total and abrupt precisely to prevent gradual accommodation.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    powerless, biographical, trapped, national).

% Religious scholars whose authority rested on direct textual access to Arabic-script Ottoman legal and theological corpora lose their interpretive monopoly. The script change is one prong of a broader disestablishment (alongside abolition of the caliphate and closure of religious schools) that structurally cannot be separated from the alphabet law's intent.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_ulema_class, payer,
    organized, generational, trapped, national).

% Calligraphers, manuscript copyists, and custodians of centuries of Arabic-script Ottoman and Islamic textual production find their skill suddenly without institutional application; the accumulated corpus becomes progressively inaccessible to new generations, severing living transmission rather than merely archiving it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, religious_manuscript_tradition_bearers, payer,
    powerless, civilizational, trapped, national).

% Older and rural citizens, least able to attend the compressed adult literacy campaigns, are permanently displaced from full civic participation in the new orthographic regime while younger urban cohorts are fully absorbed within a decade.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, elderly_and_rural_populations, payer,
    powerless, biographical, trapped, regional).

% Communities whose religious and communal life was conducted through Ottoman Turkish in Arabic script (including some non-Muslim minorities using Arabic-derived orthographies for Turkish) had no voice in the reform's design and bear disproportionate loss of intergenerational textual transmission without representation in the reform commission.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, diaspora_and_minority_communities, excluded,
    powerless, generational, trapped, regional).

% Study the reform's speed, enforcement mechanisms, and stated versus structural intent, comparing it to other 20th-century script reforms (Soviet Central Asia, Vietnam) to assess whether rupture was necessary for the stated modernization goals or was itself the primary objective.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, linguistic_and_historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, universally taught orthography, replacing regionally and religiously variable literacy pathways with one state-administered standard — genuine coordination value for administrative uniformity and mass education logistics.
% TRANSFER_FUNCTION: Moves interpretive authority, literacy capital, and civic legitimacy from the Ottoman/Islamic textual tradition and its bearers (ulema, older literate cohorts, manuscript culture) to the new secular republican state apparatus and the younger cohort that inherits fluency in the mandated script.
% ABSENT_VOICES: The ulema class, non-Muslim minority communities with their own Arabic-derived Turkish orthographic conventions, and the elderly rural majority had no seat in the Language Commission that designed and mandated the change within roughly three months of the law's passage.
% DISAPPEARANCE_RATIONALE: Had the abrupt, compulsory, single-generation transition not occurred — had script change instead unfolded gradually with bilingual transitional literacy over decades — Ottoman/Islamic textual continuity would have persisted alongside modernization, the ulema's interpretive authority would have eroded more slowly, and the state's legitimacy claim to civilizational rupture (rather than mere modernization) would not exist in its current form.
% FOUNDING_PROBLEM: The republic's founders framed the problem as: Ottoman Arabic-script Turkish was poorly suited to Turkish phonology, had high illiteracy costs, and — critically — was inseparable from an Ottoman-Islamic institutional order the new state needed to delegitimize to consolidate secular nationalist authority.
% FOUNDING_PROBLEM_CORROBORATION: Kemalist historiography and the state's own commemorative record attest the problem was purely technical/pedagogical (illiteracy, phonological mismatch). Independent linguistic historians and comparative script-reform scholars outside the Turkish state apparatus (e.g., studies of the deliberate speed and totality of implementation versus feasible gradualist alternatives such as those used in other Turkic-language reforms) attest the technical problem could have been solved without total, compulsory, near-overnight abandonment of the prior script — supporting the reading that rupture, not readability, was the operative design goal.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply in the first decade (0.62 to 0.84) as the compulsory Millet Mektepleri campaign runs its course and the older cohort's literacy capital is permanently devalued, then plateaus near 0.87-0.88 once the transition is functionally irreversible — the population that could have contested it has aged out or been absorbed. Suppression starts near its maximum (0.95) during the compulsory enforcement period (criminalization of official Arabic-script use, closure of Arabic-script presses) and declines gradually as the new orthography becomes self-sustaining through generational replacement rather than active coercion — suppression is a raw structural property here, not scaled, and its decline reflects the enforcement apparatus itself relaxing as coercion became unnecessary, not a lessening of the underlying rupture. Accessibility collapse is authored very high (0.93) because within roughly one generation, direct textual access to the prior corpus became functionally impossible for the general population — closer to the mountain end of the accessibility axis despite this being a wholly constructed, deliberate policy choice, which is exactly the kind of divergence between claim and metric the corpus is built to surface.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus's seat, this is coordination: one script, one curriculum, one national narrative, administratively rational. From the pre-reform literate population's seat, the identical mechanism operates as abrupt, non-consensual disenfranchisement with no exit — trapped exit options and powerless power atom push their computed experience toward extraction regardless of the state's stated coordination rationale. The engine should compute a marked seat divergence between kemalist_state_apparatus and pre_reform_literate_population from the same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   kemalist_state_apparatus and post_reform_educational_bureaucracy sit near the full-beneficiary end: they collect legitimacy, institutional mandate, and administrative uniformity from the rupture. pre_reform_literate_population, ottoman_ulema_class, and religious_manuscript_tradition_bearers sit near the full-target end: trapped exit options (no feasible alternative literacy path was preserved), powerless-to-organized power levels, and direct authored victim status all push d upward. new_national_literacy_elite is a genuine beneficiary but with mobile exit (their advantage is earned through adaptability, not pure capture), placing them closer to the coordination-benefiting middle than the state apparatus itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing is deliberately contested: the state's own account (technical/pedagogical necessity) if taken at face value would support a milder classification, but the founding_problem_corroboration draws on comparative script-reform scholarship suggesting the totality and speed of implementation exceeded what technical necessity required — implying the mandate (civilizational rupture) has not become obsolete but was never solely the stated one. This is not classic mandatrophy (a once-live mandate that died) but a genealogy where the stated mandate and the operative mandate diverged from the outset — the classification must not collapse this into either 'still serves its stated purpose' or 'purely obsolete,' hence founding_problem_status is authored as contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_technical_necessity,
    'Was the compulsory, near-total speed of the 1928 script transition necessary to achieve the stated literacy and phonological-fit goals, or was rapidity itself instrumental to severing continuity with the Ottoman-Islamic textual order?',
    'Comparative analysis against other 20th-century script reforms (Soviet Central Asian Turkic languages, Vietnamese quoc ngu adoption, post-Soviet Central Asian re-Latinization) that achieved similar literacy and technical goals via more gradual, less punitive transitions; examine whether a bilingual transitional period was considered and rejected in the Language Commission''s own deliberative record.',
    'If comparable technical goals were achievable via gradualism elsewhere, the totality and speed of the Turkish reform becomes strong evidence that rupture, not readability, was the primary design objective, supporting this reading''s very high ε. If technical constraints uniquely required rapid total replacement in the Turkish case, the modernization_reading''s lower ε becomes more credible for the same historical event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_technical_necessity, empirical, 'Whether transition speed/totality was technically necessary or served a rupture objective.').

omega_variable(
    kernel_reading_selection_evidence,
    'What determines which of the three sibling readings (continuity, modernization, rupture) should be treated as the primary structural account of the 1928 reform, given they are not merely differences of opinion but instantiate genuinely different beneficiary/victim structures and different ε values for what is nominally ''the same'' historical event?',
    'This is a conceptual/framing question, not resolvable by additional data alone — it depends on whether one weights the reform''s stated pedagogical justification, its documented disestablishment context (concurrent caliphate abolition, religious school closures, dress reforms), or its long-run linguistic outcomes as the diagnostic evidence for which reading is structurally dominant.',
    'Different weightings produce different primary classifications of the historical event as a whole; this story deliberately does not adjudicate between readings — it generates ONE reading (rupture) as a clean ε-invariant constraint per the authoring rule, leaving the sibling readings as separate files.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Framing under-determination among the three kernel readings of the same historical event.').

omega_variable(
    beneficiary_structure_naturalization_risk,
    'Does treating ''the post-reform state apparatus'' as a unified beneficiary obscure internal contestation within the early republic (e.g., between hardline secularists and more gradualist reformers) that might complicate the clean beneficiary/victim split this reading authors?',
    'Archival examination of internal Language Commission debates and any documented dissent within the ruling party regarding transition pace or scope.',
    'If significant internal dissent existed favoring gradualism, the beneficiary set should be narrowed to the specific faction that prevailed rather than the state apparatus as a whole, which would somewhat reduce the concentration of the authored beneficiary structure without changing the overall high-ε classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_naturalization_risk, empirical, 'Internal homogeneity of the declared beneficiary group.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__rupture_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(orth_tr_t1933, orthographic_kernel__rupture_reading, theater_ratio, 1933, 0.18).
narrative_ontology:measurement(orth_tr_t1938, orthographic_kernel__rupture_reading, theater_ratio, 1938, 0.22).
narrative_ontology:measurement(orth_tr_t1945, orthographic_kernel__rupture_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(orth_tr_t1953, orthographic_kernel__rupture_reading, theater_ratio, 1953, 0.27).
narrative_ontology:measurement(orth_tr_t1960, orthographic_kernel__rupture_reading, theater_ratio, 1960, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__rupture_reading, base_extractiveness, 1928, 0.62).
narrative_ontology:measurement(orth_be_t1933, orthographic_kernel__rupture_reading, base_extractiveness, 1933, 0.78).
narrative_ontology:measurement(orth_be_t1938, orthographic_kernel__rupture_reading, base_extractiveness, 1938, 0.84).
narrative_ontology:measurement(orth_be_t1945, orthographic_kernel__rupture_reading, base_extractiveness, 1945, 0.87).
narrative_ontology:measurement(orth_be_t1953, orthographic_kernel__rupture_reading, base_extractiveness, 1953, 0.88).
narrative_ontology:measurement(orth_be_t1960, orthographic_kernel__rupture_reading, base_extractiveness, 1960, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__rupture_reading, suppression_requirement, 1928, 0.95).
narrative_ontology:measurement(orth_su_t1933, orthographic_kernel__rupture_reading, suppression_requirement, 1933, 0.9).
narrative_ontology:measurement(orth_su_t1938, orthographic_kernel__rupture_reading, suppression_requirement, 1938, 0.85).
narrative_ontology:measurement(orth_su_t1945, orthographic_kernel__rupture_reading, suppression_requirement, 1945, 0.78).
narrative_ontology:measurement(orth_su_t1953, orthographic_kernel__rupture_reading, suppression_requirement, 1953, 0.72).
narrative_ontology:measurement(orth_su_t1960, orthographic_kernel__rupture_reading, suppression_requirement, 1960, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint files decomposing the natural-language label 'the 1928 Turkish alphabet reform' per the ε-invariance principle. orthographic_kernel__continuity_reading treats Arabic script as the carrier of Ottoman/Islamic textual continuity that the reform destroys (very high ε for religious/legal-textual continuity, near-total victim framing of the reform itself). orthographic_kernel__modernization_reading treats the reform primarily as literacy-cost and technological-fit optimization with linguistic (not textual-religious) identity substantially preserved, authoring a much lower ε. This file, rupture_reading, authors the reform as a deliberately engineered civilizational break with beneficiary = post-reform state apparatus and victim = the entire pre-reform literate population plus tradition-bearing institutions, authoring the highest ε of the three. All three share the same historical event but are structurally distinct constraints because they select different operative mechanisms (continuity destruction vs. technical modernization vs. deliberate rupture) as the constraint's defining function, each yielding a different, internally stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
