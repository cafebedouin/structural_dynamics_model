% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Kami-Buddha Ontological Unity Regime (Syncretic Fusion Reading)
 *   domain: religious/historical/political
 *
 * SUMMARY:
 *   This story instantiates the syncretic_fusion_reading of the kernel
 *   shinbutsu_ontological_substrate: on this reading, kami and buddhas are
 *   ontologically unified and honji suijaku states metaphysical truth, not
 *   merely an institutional deal. The standing arrangement under contest —
 *   the epsilon referent — is the historical shinbutsu shugo fusion complex
 *   (roughly 1040-1860 CE; interval units are ~34 years each, so t=0
 *   approximates the maturation of the honji suijaku synthesis and t=24 the
 *   eve of the 1868 separation edicts, whose demolition lies just beyond the
 *   interval and is narrated under Q5). Assessed by this reading's own
 *   lights, the arrangement mostly expresses a truth its holders are
 *   committed to, so reading-indexed extractiveness is low; the residual 0.25
 *   concedes the institutional rent even this seat acknowledges as drift
 *   rather than doctrine. The claimed_type is authored independently from the
 *   structural facts: genuine integration function, named payers, and active
 *   enforcement make this a tangled_rope from the structural seat even though
 *   the reading-indexed metrics sit low — the divergence between claim and
 *   computed per-seat types is the measurement the corpus exists to take.
 *   Sibling readings (domain_partition_reading, incoherent_bundle_reading)
 *   instantiate different constraints over the same referent and are linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - great_buddhist_temples: agenda-setter and collector (institutional/arbitrage) — administers shrines, sets doctrinal terms, receives the revenue flows
 *   - - esoteric_school_theologians: doctrinal beneficiary (institutional/identity_locked) — school identity constituted by the identification doctrine itself
 *   - - imperial_court: sponsoring beneficiary with heavy offsetting costs (institutional/constrained)
 *   - - warrior_government: secondary enforcer of the composite order (powerful/mobile)
 *   - - hereditary_shrine_priesthoods: primary bearing seat (moderate/trapped) — lineage-bound to shrines they no longer administer
 *   - - kami_primacy_traditions: doctrinal bearing seat (organized/constrained) — marginalized counter-theologians with institutional refuges
 *   - - independent_local_kami_cult_leaders: excluded seat (powerless/trapped) — definitional authority erased without consultation
 *   - - modern_religious_historiography: analytical observer (analytical/analytical) — sees the full structure including its demolition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.25).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.65).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku Kami-Buddha Ontological Unity Regime (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/historical/political").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'ea8c0257-4a20-49f6-8709-615d8dff915a').
narrative_ontology:cs_kernel_codification('ea8c0257-4a20-49f6-8709-615d8dff915a', distributed).
narrative_ontology:cs_authority_grounding('ea8c0257-4a20-49f6-8709-615d8dff915a', lineage).
narrative_ontology:cs_interpretation_layer_present('ea8c0257-4a20-49f6-8709-615d8dff915a').
narrative_ontology:cs_reading_relation('ea8c0257-4a20-49f6-8709-615d8dff915a', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('ea8c0257-4a20-49f6-8709-615d8dff915a', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('ea8c0257-4a20-49f6-8709-615d8dff915a', foundational, kami_and_buddhas_are_expressions_of_one_dharmakaya).
narrative_ontology:cs_axiom_status(kami_and_buddhas_are_expressions_of_one_dharmakaya, holdable).
narrative_ontology:cs_axiom_grounding('ea8c0257-4a20-49f6-8709-615d8dff915a', kami_and_buddhas_are_expressions_of_one_dharmakaya, theological).
narrative_ontology:cs_axiom('ea8c0257-4a20-49f6-8709-615d8dff915a', foundational, kami_are_manifest_traces_of_original_buddhas).
narrative_ontology:cs_axiom_status(kami_are_manifest_traces_of_original_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('ea8c0257-4a20-49f6-8709-615d8dff915a', kami_are_manifest_traces_of_original_buddhas, theological).
narrative_ontology:cs_axiom('ea8c0257-4a20-49f6-8709-615d8dff915a', secondary, honji_suijaku_states_metaphysical_fact_not_arrangement).
narrative_ontology:cs_axiom_status(honji_suijaku_states_metaphysical_fact_not_arrangement, holdable).
narrative_ontology:cs_axiom_grounding('ea8c0257-4a20-49f6-8709-615d8dff915a', honji_suijaku_states_metaphysical_fact_not_arrangement, theological).
narrative_ontology:cs_reference_frame('ea8c0257-4a20-49f6-8709-615d8dff915a', esoteric_nondual_ontology).
narrative_ontology:cs_drift_state('ea8c0257-4a20-49f6-8709-615d8dff915a', late_edo_kokugaku_ascendancy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ea8c0257-4a20-49f6-8709-615d8dff915a', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, great_buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, esoteric_school_theologians).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, hereditary_shrine_priesthoods).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_primacy_traditions).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, ryobu_shinto_cosmology).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, sanno_ichijitsu_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, chingo_kokka_legitimation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Head the great temple complexes (Enryaku-ji, Todai-ji, Kofuku-ji and their networks) that supply monastic administrators (betto, shuso) to major shrines, run the attached shrine-temples (jinguji), and hold the endowed estates financing joint ritual. They compose the doctrinal formulations identifying local kami with school-specific buddhas, appoint and dismiss shrine clergy, and collect a large share of shrine offerings and estate income. Their position is portable: they hold land, schools, and continental ties independent of any single shrine, and can shift personnel or doctrinal emphasis between sites at will.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, great_buddhist_temples, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, great_buddhist_temples, beneficiary).

% Scholastic lineages within Tendai and Shingon who elaborate the systematic theologies (Ryobu Shinto, Sanno ichijitsu) identifying kami with Mahavairocana and the esoteric pantheon. Their schools' distinctiveness, curricula, and initiation credentials rest on the identification doctrine; setting it aside would leave their Japan-specific theology without content. They teach, ordain, and certify shrine clergy, collecting prestige and students from the doctrine's centrality to everything they transmit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, esoteric_school_theologians, beneficiary,
    institutional, generational, identity_locked, national).

% Sponsors the unified ritual order through which the realm's protection is framed: buddha-dharma guarding the realm, kami guarding the dharma. Gains a single legible framework for ranking shrines, dispatching offerings, and binding both cults to dynastic legitimacy. Pays for the framework in tax exemptions granted to temple-shrine complexes and in the standing obligation to mediate their frequent violent disputes; precedent and the ritual calendar bind it tightly to the arrangement it funds.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).

% The Kamakura and Muromachi bakufu adjudicate shrine-temple disputes, confirm estate holdings, and at times deploy force against clerical armies to keep the composite order functioning. They enforce property settlements and public order rather than theology itself. Their posture is repositionable: they have shifted patronage between schools, taxed the temple-shrine complex, and their political heirs ultimately dismantled the entire arrangement when the calculus changed in 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_government, agenda_setter,
    powerful, generational, mobile, national).

% Hereditary kannushi lineages attached to specific shrines across generations. Under the fused order they perform the rites but answer to monastic administrators, surrender a large share of offerings and estate income to the supervising temple, and may see sons directed into monastic ranks. Their office is tied by ancestry and ritual duty to one shrine and one kami; walking away abandons the lineage's sacred charge and livelihood together.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, hereditary_shrine_priesthoods, payer,
    moderate, generational, trapped, regional).

% Intellectual and priestly movements centered on the Ise Watarai house and later the Yoshida house arguing that kami are original and buddhas derivative, or that the kami deserve definition on their own terms. They write rebuttal treatises, cultivate court and warrior patrons, and retain pockets of institutional base, but lack the enforcement reach of the temple networks. Adherents face marginalization, loss of posts, and doctrinal condemnation, though partial accommodation toward the fusion side remains open at the cost of the movement's core claim.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_primacy_traditions, payer,
    organized, generational, constrained, regional).

% Heads of village and provincial shrines outside the great temple networks whose cults were progressively redescribed in buddhist terms — kami assigned sanskrit seed-syllables, ordination rites, and temple guardianship — without anyone consulting them. Records preserve their objections chiefly as disputes over administrator appointments and fee schedules. They hold no seat in doctrinal councils and no recourse beyond petitioning the very temples that redefine their deities.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_local_kami_cult_leaders, excluded,
    powerless, biographical, trapped, local).

% Academic study, from Nativist antiquarians through Kuroda Toshio's kenmitsu taisei framework to present-day religious studies, reconstructs the fusion's formation, enforcement, and demolition from estate records, judicial proceedings, and liturgical manuals, and assesses whether the identification doctrine tracked anything real, functioned as legitimation, or both. Holds no stake in the arrangement's revenues or offices.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_religious_historiography, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, great_buddhist_temples).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two parallel religious economies — land-rooted kami cults and a translocal buddhist soteriological apparatus — into one ritual, financial, and doctrinal system: joint festivals, shared calendars, shrine-temples staffed jointly, and a single cosmology in which honoring the local kami and pursuing buddhist salvation reinforce rather than compete.
% TRANSFER_FUNCTION: Moves administrative control, offering income, and estate revenue from hereditary kami-cult lineages to supervising temples; moves doctrinal authority upward to esoteric school hierarchies; moves the kami's own status downward to trace-rank within a buddha-centered cosmos; and moves legitimation outward to court and warrior patrons who finance the composite order.
% ABSENT_VOICES: Local kami-cult leaders outside the great temple network held definitional authority before the fusion and lost it; they surface in the record mainly as disputants against betto appointments. Kami-primacy theologians wrote rebuttals but commanded no enforcement. Most fundamentally, the kami as party were spoken for entirely by buddhist interpreters — the trace had no independent seat at the table where its own nature was decided.
% DISAPPEARANCE_RATIONALE: Overnight removal of the fusion ontology and its machinery would unravel the medieval religious settlement: hundreds of shrine-temples lose staffing and purpose, temple revenues collapse, the joint festival calendar fragments, court and warrior legitimation loses its cosmological frame, and kami-primacy traditions inherit open space. This is approximately what actually happened when the 1868 separation edicts landed: haibutsu kishaku iconoclasm, mass laicization of shrine clergy, and the forced construction of a separated State Shinto rearranged the religious landscape within a decade.
% FOUNDING_PROBLEM: Two rival cultic systems — an indigenous land-and-lineage kami cult and an imported universal buddhism backed by continental statecraft — coexist in one archipelago with overlapping claims on the same villages, courts, and sacred sites; some durable settlement short of mutual annihilation must be found.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by: Nara-period state edicts regulating shrine-temple friction; paired polemics from buddhist apologists seeking accommodation and kami-cult traditionalists resisting temple encroachment, both attesting the rivalry was real; and modern historiography (Kuroda Toshio and successors) reconstructing the settlement from estate and judicial archives. Whether the problem is now dead or recurs wherever the two systems coexist remains disputed: nationalist historiography treats the Meiji separation as closure, while religious-studies scholarship documents continued informal re-synthesis in postwar practice.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.25 is reading-indexed over the fixed referent: from the fusion-believer's seat, conformity to a true ontology is alignment rather than levy, and the institutional rent layered on top (jinguji revenues, betto fees) is acknowledged as contingent drift. Suppression 0.65 is a raw structural property, unscaled by power or scope: the fusion's persistence demonstrably depended on temple leverage over shrine finance and appointments, court sanction, and bakufu adjudication — it fell within years once state power flipped in 1868, which is itself evidence of how much active enforcement it required. Theater_ratio 0.35: the joint liturgy was functionally real, but formalism grew (kami ordination rites and esoteric consecrations increasingly performed as spectacle), hence the slow rise across the grid. Accessibility_collapse 0.68: the doctrine claims totality — every kami is assimilable as a trace — collapsing independent alternatives within its world, yet inversion traditions (Watarai, Yoshida) survived at the margins, keeping collapse below mountain-grade. Resistance 0.55: lineage disputes, counter-theologies, and finally the kokugaku crescendo met the arrangement continuously. All three tracked series share one grid ({0,4,8,12,16,20,24}); the suppression_requirement series is authored because the story specifically traces enforcement machinery hardening through the medieval period and plateauing late — its catastrophic post-1868 fall sits beyond the interval endpoint. Coalition note: the bearing seats were not purely powerless — shrine lineages repeatedly formed litigation coalitions through bakufu courts and occasionally won temporary rollbacks of temple control, which is why neither payer seat is authored at the powerless atom.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications should diverge sharply. From the temple seat (agenda_setter, arbitrage exit) the arrangement computes as coordination it built and profits from; from the identity_locked theologian seat it computes as near-pure subsidy, since the doctrine constitutes the assessor's own institutional identity; from the trapped priesthood seat the identical structure computes as administered extraction; from the constrained kami_primacy seat as enforced doctrinal dispossession with partial shelter; from the court and warrior seats as near-symmetric stewardship of an order they police and pay for. The engine computes these divergences from the authored power/exit/role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: great_buddhist_temples (agenda_setter + beneficiary, arbitrage) derive d near the beneficiary end — enforcement costs they bear are investments, not levies; esoteric_school_theologians (beneficiary, identity_locked) sit nearest the full-beneficiary end, their exit being unthinkable without dissolving their schools; imperial_court derives low d from its beneficiary declaration, but its true position is more symmetric — it consumed legitimation while paying exemptions and mediation costs; no directionality_override is declared because an override keyed to the institutional power atom would also strike the temples and theologians and distort them, so the court's ambivalence is documented here and left to qualitative analysis; warrior_government (agenda_setter, mobile) sits near symmetric as a paid enforcer; hereditary_shrine_priesthoods (payer, trapped) derive near the full-target end; kami_primacy_traditions (payer, constrained) derive high d, tempered slightly by institutional refuges; independent_local_kami_cult_leaders are excluded-seat witnesses whose erasure the derivation registers only indirectly, since they appear in neither the beneficiary nor the victim arrays — a known limitation noted for downstream analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against both available mislabels. From inside the fusion seat the arrangement presents as pure truth-coordination — a rope claim; from the kokugaku and Kuroda-style outsider seat it presents as pure imposition sustained by enforcement — a snare claim. The structural data support the hybrid: a real integration function (one ritual-financial-cosmological system replacing two rival ones) coexisting with named payers and active enforcement. On mandatrophy proper: unlike a piton, the fusion did not outlive its mandate as inert performance — its mandate (settling the kami-buddha rivalry) remained live until the mandate itself was abolished by state fiat in 1868, killing arrangement and problem together. Hence founding_problem_status is authored contested rather than dead, theater_ratio plateaus at a moderate 0.35 rather than dominating, and the disappearance verdict is world_rearranges rather than world_unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story is one reading (syncretic_fusion_reading) of kernel shinbutsu_ontological_substrate; what structurally changes if either sibling reading is adopted instead?',
    'Compare the sibling files directly: adopting domain_partition_reading replaces this constraint with one whose victim set shrinks (functional complementarity dispossesses no doctrinal tradition) and whose enforcement data drop out; adopting incoherent_bundle_reading dissolves the kernel into separate institutional-drift stories with no unified epsilon and no foreclosure edges. The disagreement is located in the truth-value of the ontological-unity claim itself.',
    'Classification, epsilon, beneficiary/victim structure, and the cs_structure foreclosure relations all shift with the adopted reading; this file''s values are valid only under the fusion reading''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one kernel, three readings, this file is the maximal-commitment instantiation').

omega_variable(
    two_truths_tiering,
    'Do the foreclosure relations to both siblings survive if their claims are read as conventional-tier descriptions inside a two-truths (ultimate/conventional) framework?',
    'Test whether any single party''s framework can hold a sibling''s claim AS ITS ANSWER TO THE SUBSTRATE QUESTION while retaining this reading''s ultimate-tier unity claim; if tiering absorbs the siblings, the relations degrade from forecloses to coexists_with and the sibling readings become live options within one framework.',
    'Foreclosure computation and downstream commitment-system pattern outputs change; the siblings would no longer be logically eliminable by this reading, weakening the deep-commitment structural delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_truths_tiering, conceptual, 'Whether the reading''s assertive truth-claim forecloses siblings flatly or only modulo two-truths tiering').

omega_variable(
    seat_epsilon_circularity,
    'Does the low reading-indexed extractiveness reflect the arrangement''s genuine benignity, or the fact that the seat doing the assessing (esoteric fusion believers) is also a principal beneficiary?',
    'Score the same referent from non-beneficiary testimony — hereditary priesthood dispute records, kami-primacy polemic, kokugaku attack literature — and check convergence; agreement across seats on low extraction would vindicate the reading-indexed value, divergence would indicate beneficiary-grade motivated perception inflating the subsidy reading.',
    'If circular, effective extraction is materially understated and the computed classification shifts toward the snare boundary despite the tangled_rope claim; per-seat divergence already computed by the engine becomes the decisive evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seat_epsilon_circularity, empirical, 'Assessor-beneficiary circularity risk in the reading-indexed epsilon').

omega_variable(
    suppression_structural_internalized,
    'Was the fusion''s coercive grip structural (temple leverage over shrine finance, appointments, and court sanction) or internalized (practitioners'' sincere nondual devotion making alternatives unthinkable)?',
    'Post-1868 natural experiment: once the state removed all structural enforcement and penalized fused practice, did fused devotion persist underground and re-emerge after liberalization (internalization), or vanish pending legality (structure)? Both trajectories are documented regionally; comparative parish and diocesan records resolve the proportions.',
    'A large internalized share means the structural suppression metric understates the total constraint force believers carried after enforcement ended, and the classification of the post-demolition residue shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized, empirical, 'Structural versus internalized suppression mechanism in the fusion regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t4, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement_basis(shin_tr_t4, observed).
narrative_ontology:measurement(shin_tr_t8, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(shin_tr_t8, observed).
narrative_ontology:measurement(shin_tr_t12, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(shin_tr_t12, observed).
narrative_ontology:measurement(shin_tr_t16, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(shin_tr_t16, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t24, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(shin_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t4, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement_basis(shin_be_t4, observed).
narrative_ontology:measurement(shin_be_t8, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement_basis(shin_be_t8, observed).
narrative_ontology:measurement(shin_be_t12, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement_basis(shin_be_t12, observed).
narrative_ontology:measurement(shin_be_t16, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement_basis(shin_be_t16, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t24, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement_basis(shin_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t4, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement_basis(shin_su_t4, observed).
narrative_ontology:measurement(shin_su_t8, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement_basis(shin_su_t8, observed).
narrative_ontology:measurement(shin_su_t12, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(shin_su_t12, observed).
narrative_ontology:measurement(shin_su_t16, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(shin_su_t16, observed).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(shin_su_t20, observed).
narrative_ontology:measurement(shin_su_t24, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement_basis(shin_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, meiji_shinbutsu_bunri_settlement).

% DUAL FORMULATION NOTE:
% Constraint family over one referent (the shinbutsu shugo fusion arrangement, c. 1040-1860), three readings, three epsilons: this file authors the syncretic_fusion_reading (low reading-indexed epsilon; deep ontological commitment); domain_partition_reading authors the functionalist account (moderate epsilon; no doctrinal victims); incoherent_bundle_reading authors the debunking decomposition (highest epsilon; enforcement without kernel). The upstream member by empirical confidence is domain_partition_reading (its functional claims are least contested); this reading exerts structural pressure on both siblings via its truth-claim (foreclosure edges in cs_structure), and all three are downstream of the same historical enforcement record that meiji_shinbutsu_bunri_settlement terminated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
