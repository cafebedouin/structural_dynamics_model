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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic-Script Regime for Turkish — Ottoman Continuity Reading
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story instantiates the ottoman_continuity_reading of the
 *   script_as_identity kernel: the claim that Arabic script is constitutive
 *   of Turkish-Islamic identity and historical continuity. The standing
 *   arrangement under contest — and therefore the ε referent — is the
 *   Arabic-script regime for Turkish as it actually operated from the
 *   Tanzimat to the 1928 reform: enforced script uniformity, licensed
 *   printing, medrese-gated literacy, and repeated suppression of
 *   orthographic alternatives. The reading's own rhetoric presents the
 *   arrangement as pure coordination (a community bound to its revelation and
 *   its memory); the authored metrics describe its actual operation
 *   independently, and they show a genuine coordination function carrying
 *   substantial asymmetric extraction. KEY AGENTS (by structural
 *   relationship): ulema_religious_establishment — primary beneficiary and
 *   co-enforcer (institutional/identity_locked); scribal_bureaucracy_kalemiye
 *   — secondary beneficiary (organized/constrained);
 *   calligraphic_and_print_trades — tertiary beneficiary
 *   (moderate/identity_locked); ottoman_state_education_authorities — agenda
 *   setter bearing the arrangement's fiscal costs
 *   (institutional/constrained); turkish_speaking_masses — primary target
 *   (powerless/trapped); latin_alphabet_proposers — secondary target
 *   (moderate/constrained); minority_script_turkish_printers — marginal
 *   target with partial exit (organized/mobile); mass_literacy_advocates —
 *   excluded voice; european_orientalist_linguists — analytical observer. The
 *   colloquial label 'the Turkish script question' decomposes into three
 *   structurally distinct constraints (this reading,
 *   kemalist_rupture_reading, phonetic_instrumentalism_reading); each carries
 *   its own ε, victims, and type, linked through network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.75).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.85).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic-Script Regime for Turkish — Ottoman Continuity Reading").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, '6b3cac77-e492-42aa-ba64-22492e8e5939').
narrative_ontology:cs_kernel_codification('6b3cac77-e492-42aa-ba64-22492e8e5939', formalized).
narrative_ontology:cs_authority_grounding('6b3cac77-e492-42aa-ba64-22492e8e5939', lineage).
narrative_ontology:cs_interpretation_layer_present('6b3cac77-e492-42aa-ba64-22492e8e5939').
narrative_ontology:cs_reading_relation('6b3cac77-e492-42aa-ba64-22492e8e5939', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b3cac77-e492-42aa-ba64-22492e8e5939', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('6b3cac77-e492-42aa-ba64-22492e8e5939', foundational, script_carries_revelational_continuity).
narrative_ontology:cs_axiom_status(script_carries_revelational_continuity, holdable).
narrative_ontology:cs_axiom_grounding('6b3cac77-e492-42aa-ba64-22492e8e5939', script_carries_revelational_continuity, theological).
narrative_ontology:cs_axiom('6b3cac77-e492-42aa-ba64-22492e8e5939', foundational, civilizational_memory_requires_script_continuity).
narrative_ontology:cs_axiom_status(civilizational_memory_requires_script_continuity, holdable).
narrative_ontology:cs_axiom_grounding('6b3cac77-e492-42aa-ba64-22492e8e5939', civilizational_memory_requires_script_continuity, instrumental).
narrative_ontology:cs_reference_frame('6b3cac77-e492-42aa-ba64-22492e8e5939', quranic_ottoman_transmission_continuity).
narrative_ontology:cs_drift_state('6b3cac77-e492-42aa-ba64-22492e8e5939', post_1928_republican_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('6b3cac77-e492-42aa-ba64-22492e8e5939', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, scribal_bureaucracy_kalemiye).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, calligraphic_and_print_trades).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, turkish_speaking_masses).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, latin_alphabet_proposers).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, minority_script_turkish_printers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of Qur'anic and Ottoman learning across the ilmiye hierarchy. Script mastery is their credential: they certify teachers through ijaza chains, run the medrese instruction through which nearly all literacy passes, and issue fatwas against orthographic innovation. Their interpretive authority over the written word — including scripture itself — flows from the script remaining in their custody. Leaving the arrangement would mean dissolving the authority structure they are.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment, beneficiary,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment, agenda_setter).

% Staffs the chanceries, tax registers, and courts of the empire. Careers, pensions, and family standing are built on decades of script acquisition that outsiders cannot quickly replicate. An orthographic change would strand their specific human capital; some individuals privately concede the script's inefficiency while defending it professionally, and a minority later transitions into Latin-script republican service.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, scribal_bureaucracy_kalemiye, beneficiary,
    organized, biographical, constrained, continental).

% Hattats, illuminators, binders, and holders of the limited licenses to print in the script. Livelihoods ride on the manuscript and licensed-print economy; calligraphic formation is a decade-long apprenticeship that fuses craft skill with devotional identity. Licensed operators collect scarcity income from the state's restriction of competing presses.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, calligraphic_and_print_trades, beneficiary,
    moderate, biographical, identity_locked, regional).

% The Porte and, after 1846, the Ministry of Education mandate script uniformity in schools and chanceries, license presses, and periodically commission orthographic studies — which are then shelved under religious-political pressure. The state pays real costs for the arrangement: slow chancery throughput, diplomatic isolation from a Latin-letter world, and chronic under-literacy in a mass-conscription era. Replacing the script wholesale would rupture the state's alliance with religious authority, so successive governments patch rather than fix.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_state_education_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% The overwhelming majority of Turkish speakers. Written literacy requires years of instruction available almost exclusively through religious and state channels concentrated in towns; contracts, courts, news, and office remain behind the script barrier. Families bear the instructional burden for sons who proceed far enough to profit from it; daughters and rural children largely do not. There is no exiting the linguistic community, and no channel through which their preference about the script's form is registered.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, turkish_speaking_masses, payer,
    powerless, generational, trapped, continental).

% Officials, officers, and intellectuals who propose phonetic or Latin orthographies from the 1860s onward. Proposals meet dismissal, censorship, and career damage; several publish from exile in Cairo, Paris, or Geneva. After 1908 they re-enter open debate, and a number defect openly to the reform camp once enforcement weakens.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, latin_alphabet_proposers, payer,
    moderate, biographical, constrained, continental).

% Karamanli (Greek-letter) and Armeno-Turkish printers who produce Turkish in non-Arabic scripts for their millet communities. They operate at the margin of the arrangement, periodically suspected of divisive intent, with smaller markets and precarious legal standing. Unlike the Muslim majority they possess a partial exit: communal networks and alphabets of their own absorb some of the cost.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, minority_script_turkish_printers, payer,
    organized, biographical, mobile, regional).

% Educators, army officers, and provincial administrators who argue that no mass schooling program can succeed on a script that takes years to acquire. They stand outside the Istanbul deliberating circles; their case enters official debate only episodically, usually in wartime crisis, and recedes when the fiscal emergency passes.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, mass_literacy_advocates, excluded,
    moderate, biographical, constrained, national).

% Missionary linguists, consular translators, and academic orientalists who document the script question from outside: they catalog the Ottoman corpus, measure literacy, compare orthographies, and advise on transliteration. They hold no stake in Ottoman domestic politics, and their comparative findings circulate back into Ottoman debate as ammunition for both defenders and reformers.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, european_orientalist_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single Arabic-based orthography gave a continental, multi-confessional empire one written medium for law, administration, commerce, and religion, and kept a millennium-deep corpus of Qur'anic, Perso-Islamic, and Ottoman texts continuously readable by the trained class; script mastery defined and reproduced the literate stratum that staffed the state.
% TRANSFER_FUNCTION: Moves years of instructional labor from students and families into the script-acquisition pipeline controlled by medrese and state schools; moves literacy itself — with the offices, income, and standing attached to it — from the general population to the small trained class; moves interpretive authority over the written word to the ulema and scribes who hold the keys.
% ABSENT_VOICES: The illiterate majority whose children bore the acquisition cost never entered any deliberative forum; rural populations, women, and non-elites were absent from the Istanbul-centered debates; several orthographic reformers argued from exile after domestic publication became unsafe. Deliberation was confined to script-literate elites whose livelihoods the arrangement sustained — the consensus in defense of the script formed inside a room the arrangement had already filtered.
% DISAPPEARANCE_RATIONALE: If the Arabic-script mandate vanished overnight around 1900, mass-schooling advocates would pivot to teachable orthographies, the scribal class's licensed position would erode within a generation, the ulema would lose their gatekeeping place over the written word, and the chanceries would reorganize around whatever script the newly schooled generation learned. The arrangement, not the language, was holding the empire's entire literate order in place.
% FOUNDING_PROBLEM: Before mass print and mass schooling, a continental empire needed one prestigious written medium to bind diverse provinces to a single legal-administrative and religious corpus, and a reliable way to reproduce the trained class that could operate it; the Arabic-script regime solved both problems at once.
% FOUNDING_PROBLEM_CORROBORATION: European orientalist surveys, missionary literacy reports, and later Turkological scholarship independently document both halves: the corpus-continuity value of the script and the severity of the literacy bottleneck. Republican pedagogues and the 1928 reform commission attest that the imperial-administrative founding problem was dead with the empire. The identity-constitutivity claim itself, however, is attested almost exclusively by the beneficiary classes and their heirs — ulema, scribal families, calligraphic guilds — and that asymmetry in corroboration is itself signal.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is substantial and rises across the interval (0.58 to 0.75) because the arrangement's cost profile worsens as print capitalism, mass conscription, and mass schooling raise the opportunity price of a hard script gated through religious instruction — extraction accumulates on a functioning coordination core rather than replacing it. Suppression is high (0.85 at interval end) and is a raw structural property, unscaled by power or scope: press licensing, censorship of orthographic proposals, fatwa-backed condemnation, and career destruction of reformers. Theater stays low-to-moderate (0.10 to 0.28) because enforcement was functionally real for most of the interval; the late rise marks the defensive turn — by 1924-1928 the traditionalist case is increasingly rhetorical performance layered over collapsing control. The 1908 dip in suppression_requirement records the post-revolutionary press-freedom window, when the enforcement burden briefly lightened before wartime centralization re-hardened it; the series is otherwise monotone and no full oscillatory cycle is modeled. All three tracked metrics share one eight-point grid (1839, 1863, 1880, 1897, 1908, 1918, 1924, 1928), each value asserted at every shared point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural data. From the turkish_speaking_masses seat the arrangement is a barrier that prices literacy in years and hands the written word to a credentialed few; from the ulema seat the same arrangement is custodianship of revelation and unbroken transmission; from the state seat it is legitimacy infrastructure whose wholesale replacement risks more than its known costs — until the 1918-1922 collapse reverses that calculus and the successor regime chooses rupture. The engine computes these per-seat classifications from the declared power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the ulema, kalemiye, and calligraphic trades toward the beneficiary end of d; victim declarations drive the masses, proposers, and minority printers toward the target end. Exit modulates within the target side: the trapped masses sit nearest full-target, the constrained proposers slightly inside them, and the mobile minority printers — who can shift output into millet networks and non-Arabic alphabets — materially further back. One nuance deserves note: identity_locked normally signals a target-side agent, but here it locks beneficiaries into the arrangement (the ulema and calligraphers cannot leave without dissolving the authority and craft they are), which stabilizes enforcement rather than amplifying their extraction; the derivation handles this through the beneficiary declaration rather than through an override, so no directionality_overrides entries are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents symmetric mislabelings. Reading the identity rhetoric at face value would score the arrangement as pure coordination and miss the literacy gatekeeping; reading it purely as clerical extraction would miss the real coordination function — one script binding a continental administration to a millennium-deep readable corpus — that made the arrangement worth defending and worth attacking. Tangled_rope holds both halves visibly. On genealogy: the founding problem (imperial administration plus unified learned culture) dissolved with the empire itself, and the arrangement was terminated by regime rupture in 1928 rather than decaying into performance — so no piton drift occurs inside this interval, and mandatrophy_resolved is deliberately left undeclared. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) correctly raises no zombie flag: the continuity-of-memory half of the founding problem remains genuinely live for religious communities, while the imperial-administrative half is dead. Any successor story tracking the reading's post-1928 afterlife in minority religious and diaspora communities should expect a high theater_ratio and a commemorative mandate — that is a different constraint, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story is one reading (ottoman_continuity_reading) of the script_as_identity kernel; how would the sibling readings re-author epsilon, victim sets, and type over the same historical material?',
    'Generate and compare the sibling stories: kemalist_rupture_reading authors the same standing arrangement from the rupture seat (expected: higher epsilon, the masses and the future nation as victims, snare-or-tangled_rope verdict); phonetic_instrumentalism_reading authors it from the neutral-technology seat (expected: epsilon concentrated on the forgone efficiency surplus, type driven by enforcement overhead rather than identity).',
    'If the siblings converge on this story''s tangled_rope verdict with different directionalities, the kernel contest is evaluative rather than structural; if they diverge on type, the colloquial label hides structurally distinct constraints and the family decomposition is load-bearing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression was structural (licensing, censorship, gated instruction) versus internalized (piety-fused conviction that altering the script of revelation is sacrilege)?',
    'Post-1928 trajectory: structural enforcement flipped overnight with the regime, yet Arabic-script loyalty persisted for decades in religious communities, Qur''anic pedagogy, and diaspora — persistence after barrier removal indicates a real internalized component; its decay rate estimates the share.',
    'If internalized suppression is large, the arrangement''s effective hold exceeded its enforcement machinery, and the 1928 reform''s speed is explained by the regime change breaking the identity frame rather than by lifting barriers; if small, the machinery was the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized shares of the script regime''s suppressive force.').

omega_variable(
    literacy_barrier_attribution,
    'Was the literacy bottleneck caused by the script itself, or by the state''s refusal to fund mass schooling under any orthography?',
    'Comparative analysis of contemporaneous orthographies of comparable difficulty (Persian, Urdu, Bosnian Arebica) against their societies'' literacy outcomes, controlling for school expenditure; and post-1928 Turkish literacy curves decomposed into script-effect and schooling-expansion effects.',
    'If script difficulty explains little of the gap, this story''s extractiveness is overstated and the arrangement''s costs were chiefly fiscal neglect — shifting weight toward the state seat''s responsibility; if script difficulty explains much, the extraction attribution to the script regime stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_barrier_attribution, empirical, 'Confound decomposition: script difficulty versus schooling investment in the literacy outcome.').

omega_variable(
    continuity_neutrality_near_contradiction,
    'Can the continuity reading and the phonetic_instrumentalism_reading genuinely coexist, or does holding one commit a party to rejecting the other''s core premise?',
    'Examine actual held positions: many Turkish religious conservatives write Turkish in Latin letters while insisting on Arabic for scripture — a domain-separated position that satisfies both readings by partitioning scope. If such partitioned frameworks are stable and widespread, the readings coexist by scope-splitting; if partisans uniformly refuse partition, the relation trends toward foreclosure within any single framework.',
    'If scope-partitioned coexistence is the norm, the reading_relations edge to phonetic_instrumentalism_reading is correctly coexists_with; if partition collapses under scrutiny, the edge should be revisited as a foreclosure candidate and the engine''s axiom-contradiction computation will register it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_neutrality_near_contradiction, conceptual, 'Whether constitutivity and neutrality premises can share a framework by domain partition.').

omega_variable(
    beneficiary_load_bearing_vs_popular_attachment,
    'Was the arrangement''s persistence sustained primarily by concentrated beneficiary enforcement (ulema, scribes, licensed trades) or by broad popular attachment to the script as identity?',
    'Trace enforcement events to their sponsors: fatwas, licensing decisions, and censorship originate in the beneficiary classes; survey and literary evidence for lay attachment (devotional handwriting culture, resistance to the 1928 reform outside elite circles) tests the popular leg.',
    'If enforcement was overwhelmingly beneficiary-driven, the arrangement is closer to captured coordination with passive subjects; if popular attachment was load-bearing, the coordination function runs deeper and the extraction reading must discount accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_load_bearing_vs_popular_attachment, empirical, 'Relative weight of elite enforcement versus diffuse popular attachment in persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1839, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1839, script_as_identity__ottoman_continuity_reading, theater_ratio, 1839, 0.1).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1863, script_as_identity__ottoman_continuity_reading, theater_ratio, 1863, 0.11).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1880, script_as_identity__ottoman_continuity_reading, theater_ratio, 1880, 0.13).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1897, script_as_identity__ottoman_continuity_reading, theater_ratio, 1897, 0.14).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1908, script_as_identity__ottoman_continuity_reading, theater_ratio, 1908, 0.16).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1918, script_as_identity__ottoman_continuity_reading, theater_ratio, 1918, 0.2).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1924, script_as_identity__ottoman_continuity_reading, theater_ratio, 1924, 0.24).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t1928, script_as_identity__ottoman_continuity_reading, theater_ratio, 1928, 0.28).

% Extraction over time
narrative_ontology:measurement(ottoman_continuity_reading_be_t1839, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1839, 0.58).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1863, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1863, 0.61).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1880, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1880, 0.64).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1897, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1897, 0.66).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1908, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1908, 0.69).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1918, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1918, 0.72).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1924, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1924, 0.74).
narrative_ontology:measurement(ottoman_continuity_reading_be_t1928, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1928, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(ottoman_continuity_reading_su_t1839, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1839, 0.6).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1863, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1863, 0.64).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1880, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1897, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1897, 0.72).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1908, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1908, 0.63).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1918, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1918, 0.74).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1924, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1924, 0.81).
narrative_ontology:measurement(ottoman_continuity_reading_su_t1928, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1928, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Turkish script question' decomposes into three structurally distinct constraints sharing the script_as_identity kernel. This story (ottoman_continuity_reading) authors epsilon for the standing Arabic-script arrangement as the continuity seat sees it: a real coordination core (corpus access, imperial unity, religious transmission) carrying substantial asymmetric extraction. kemalist_rupture_reading evaluates the same historical material from the rupture seat, for which the arrangement's defining feature is the severance it imposed and the modernization it blocked — expect a different epsilon and victim emphasis. phonetic_instrumentalism_reading strips the identity layer entirely and evaluates the script as encoding technology, locating costs in forgone phonetic efficiency and enforcement overhead. The upstream/downstream structure runs from this reading (highest empirical confidence about what the arrangement was) toward the siblings (evaluative and technical re-readings); each family member links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
