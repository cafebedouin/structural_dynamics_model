% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: 1928 Turkish Script Reform — Rupture Reading
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish script reform replaced the Arabic-based Ottoman script
 *   with a modified Latin alphabet in a matter of months, enacted by law
 *   under Atatürk's single-party regime. The rupture_reading interprets this
 *   not as modernization or practical necessity but as a deliberate cultural
 *   severance: the new script made the entire Ottoman-Islamic textual
 *   tradition (centuries of literature, law, theology, science, private
 *   correspondence) inaccessible to the new generation without mediated
 *   translation. The pre-reform literate population (victims) lost direct
 *   access to their own cultural heritage; the post-reform state apparatus
 *   (beneficiaries) gained a controllable semantic field where national
 *   identity could be rewritten. The constraint persists because the script
 *   change is irreversible — the Ottoman script is not returning — and the
 *   cultural rupture it created is now constitutive of Turkish national
 *   identity. The claimed type is Snare: pure extraction of cultural
 *   continuity with coordination (literacy, modernization) as cover.
 *
 * KEY AGENTS:
 *   - post_reform_state_apparatus: Primary beneficiary (institutional/arbitrage) — controls the new semantic field, collects legitimacy from rupture
 *   - kemalist_ideological_institutions: Beneficiary (institutional/generational) — universities, military, CHP party apparatus that administer the rupture narrative
 *   - pre_reform_literate_population: Primary victim (organized/identity_locked) — entire generation cut off from its own textual heritage; exit requires learning a new script AND accepting the rupture narrative
 *   - ottoman_islamic_scholarly_class: Victim (powerless/trapped) — ulema, medrese teachers, Sufi orders whose authority rested on Arabic-script textual mastery; structurally eliminated
 *   - religious_education_institutions: Victim (organized/trapped) — medreses, tekkes, vakıf libraries; legally dissolved or rendered non-functional by script change
 *   - cultural_continuity_practitioners: Victim (moderate/identity_locked) — poets, historians, calligraphers, manuscript scholars; forced into translation labor or silence
 *   - republican_elite: Beneficiary (powerful/arbitrage) — the cohort that engineered the rupture and staffed the new institutions; dual-positioned as both architects and inheritors
 *   - post_1950_opposition_politicians: Excluded (organized/constrained) — Democrat Party and successors who could not contest the script without contesting the republic's founding act; structurally silenced on this axis
 *   - contemporary_turkish_citizens: Observer (analytical/analytical) — inherit the rupture as natural fact; the constraint appears as Mountain from this seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.92).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.88).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "1928 Turkish Script Reform — Rupture Reading").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '4af05709-3463-4b28-a007-0b2dd9f824d3').
narrative_ontology:cs_kernel_codification('4af05709-3463-4b28-a007-0b2dd9f824d3', formalized).
narrative_ontology:cs_authority_grounding('4af05709-3463-4b28-a007-0b2dd9f824d3', extraction).
narrative_ontology:cs_interpretation_layer_present('4af05709-3463-4b28-a007-0b2dd9f824d3').
narrative_ontology:cs_reading_relation('4af05709-3463-4b28-a007-0b2dd9f824d3', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('4af05709-3463-4b28-a007-0b2dd9f824d3', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('4af05709-3463-4b28-a007-0b2dd9f824d3', foundational, cultural_severance_as_national_birth).
narrative_ontology:cs_axiom_status(cultural_severance_as_national_birth, holdable).
narrative_ontology:cs_axiom_grounding('4af05709-3463-4b28-a007-0b2dd9f824d3', cultural_severance_as_national_birth, deontological).
narrative_ontology:cs_axiom('4af05709-3463-4b28-a007-0b2dd9f824d3', secondary, script_as_sovereign_boundary_marker).
narrative_ontology:cs_axiom_status(script_as_sovereign_boundary_marker, holdable).
narrative_ontology:cs_axiom_grounding('4af05709-3463-4b28-a007-0b2dd9f824d3', script_as_sovereign_boundary_marker, conventional).
narrative_ontology:cs_reference_frame('4af05709-3463-4b28-a007-0b2dd9f824d3', revolutionary_rupture_1928).
narrative_ontology:cs_drift_state('4af05709-3463-4b28-a007-0b2dd9f824d3', contemporary_post_kemalist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4af05709-3463-4b28-a007-0b2dd9f824d3', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, republican_elite).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_ideological_institutions).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_islamic_scholarly_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, religious_education_institutions).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, cultural_continuity_practitioners).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, national_identity_requires_script_severance).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, cultural_rupture_as_state_founding_act).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, script_as_political_boundary_marker).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1928 script law, controls all public education, printing, bureaucracy, and legal publication in the new script. Collects legitimacy rents as the 'founder' of modern Turkish identity. Can revise the constraint (e.g., permit Ottoman script education) but chooses not to because the rupture is its founding legitimacy. Exit is arbitrage-grade — it could open the script field tomorrow with a cabinet decision.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_state_apparatus, beneficiary).

% Universities (especially language/history faculties), Turkish Language Association (TDK), Turkish Historical Society, military academies, CHP party apparatus. Their institutional identity and curricula are built on the rupture narrative. They benefit from controlling the official history and language planning. Exit is constrained — abandoning the rupture narrative would delegitimize their institutional mission, but they could adapt incrementally.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_ideological_institutions, beneficiary,
    institutional, generational, constrained, national).

% The 1920s-30s cohort that engineered the reform: Atatürk, İnönü, Atatürk's ministers, early TDK members. They authored the constraint and directly collected its status rents. Dual-positioned as architects (agenda_setter) and inheritors (beneficiary). Exit was arbitrage-grade for them — they could have chosen a different path in 1928 but chose rupture. Their descendants inherit the beneficiary position without the agency.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, republican_elite, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, republican_elite, agenda_setter).

% The entire generation (approx. 10-15% literacy rate in 1927, concentrated in urban men, religious scholars, bureaucrats, merchants) who could read Ottoman Turkish in Arabic script. They lost direct access to: their own family letters, property records, religious texts, literature, newspapers, scientific works. Forced to either learn the new script (costly, ideologically loaded) or depend on state-mediated translations. Exit is identity_locked — their self-concept as 'literate Muslims/Ottomans' is constituted through the very textual tradition the constraint severs. Learning the new script feels like betraying that identity; not learning it means illiteracy in the new order.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    organized, biographical, identity_locked, national).

% Ulema (Islamic judges/scholars), medrese (theological school) teachers, muftis, Sufi sheikhs, vakıf (endowment) administrators. Their entire professional authority, legal reasoning, and spiritual practice depended on mastery of Arabic-script Islamic texts (Quran, hadith, fiqh, tasawwuf). The 1924 abolition of the caliphate and 1925 closure of tekkes (Sufi lodges) preceded the script reform; the script change completed their structural elimination by making their textual capital illegible to the next generation. No exit — their institutions were legally dissolved, their professional status revoked, their textual heritage rendered inaccessible.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_islamic_scholarly_class, payer,
    powerless, biographical, trapped, national).

% Medreses (Ottoman theological colleges), tekkes (Sufi lodges), vakıf libraries, mekteps (elementary religious schools). The 1924 Tevhid-i Tedrisat Law (Unification of Education) placed all education under the Ministry of Education; the 1928 script law made their Arabic-script curricula illegal. Their libraries (millions of manuscripts) were transferred to state institutions or left to decay. No exit — the legal framework prohibited their operation, and the script change ensured no new generation could maintain them.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, religious_education_institutions, payer,
    organized, generational, trapped, national).

% Ottoman poets, calligraphers, manuscript copyists, historians, chroniclers, private scholars, families with manuscript collections. Their craft and identity were embedded in the Arabic-script tradition. Forced into translation labor (rendering Ottoman works into Latin script for state publishers) or silence. Some preserved manuscripts privately at great risk. Exit is identity_locked — calligraphy is not a transferable skill; Ottoman paleography requires the script. The constraint did not just change their tools; it made their epistemic world invisible.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, cultural_continuity_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Democrat Party (1950-60), Justice Party (1960s-70s), National Salvation Party (1970s), Welfare Party (1990s), AK Party (2000s+). Each opposition wave represented constituencies with Ottoman-Islamic cultural memory. They could not contest the script reform without appearing to contest the republic's founding. Some permitted limited Ottoman script education (imam-hatip schools, university electives) but never challenged the Latin script monopoly. Their exclusion is structural: the rupture_reading makes the script change synonymous with the republic itself.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_1950_opposition_politicians, excluded,
    organized, biographical, constrained, national).

% Born into a Latin-script Turkey where the reform is taught as a natural, necessary modernization. The Ottoman script appears as decorative calligraphy or historical curiosity, not a living literacy. From this seat the constraint looks like a Mountain — 'Turkish has always been written this way.' The extraction and suppression are invisible because the victims are dead and the beneficiaries are the state itself. This is the false_summit_mountain seat: if the engine ever classifies this constraint as Mountain from any seat, this observer seat is where it happens.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, contemporary_turkish_citizens, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a uniform, phonemic script for Turkish that enabled mass literacy campaigns, standardized education, and integration with Western scientific/technical print culture — solving the coordination problem of a multi-script, multi-dialect empire transitioning to a nation-state.
% TRANSFER_FUNCTION: Transferred cultural capital (direct access to 600 years of Ottoman textual production) from the pre-reform literate population and scholarly institutions to a state-controlled translation/interpretation monopoly. The state became the sole authorized gatekeeper of what the Ottoman past means, filtering it through the new script and republican ideology.
% ABSENT_VOICES: The pre-reform literate population (dead by 1960s), Ottoman scholarly class (structurally eliminated), and religious education institutions (legally dissolved) are physically absent. Their would-be successors (Islamic intellectuals, traditionalist scholars, manuscript specialists) are excluded from the legitimate discourse by the rupture_reading's equation of script reform with the republic itself — to question the script is to question the nation.
% DISAPPEARANCE_RATIONALE: If the script law and its enforcement vanished overnight: Ottoman script education would re-emerge in religious and cultural institutions; manuscript libraries would become directly accessible; a dual-script public sphere would develop; the state's monopoly on interpreting the Ottoman past would break; the republican elite's founding legitimacy would face its first genuine challenge. The cultural rupture would become a contested choice rather than a settled fact.
% FOUNDING_PROBLEM: The Ottoman Empire's collapse created a legitimacy vacuum: the new Turkish state needed a national identity that could survive in a Western-dominated international system, unify a heterogeneous Anatolian population, and break the religious-legal authority of the ulema who had opposed the nationalist movement. The script reform was engineered as the irreversible act that would make the new nation legible to itself and the West, and the old order illegible.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is attested by: (1) Turkey's 1945 UN founding membership and 1952 NATO accession — international recognition achieved; (2) 1950 peaceful democratic transition — internal legitimacy consolidated; (3) 1980s-90s economic integration with Europe — modernization achieved without script reversal; (4) Independent historians (Erik Jan Zürcher, Şerif Mardin, Niyazi Berkes) documenting that the nation-state consolidation was complete by 1960. The republican elite (beneficiaries) contest this, citing ongoing threats to 'national unity' — but no non-beneficiary source corroborates the founding problem as live.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is extremely high (0.92 at origin, declining to 0.72 by 2000) because the constraint transfers an entire cultural inheritance from the pre-reform population to a state-controlled translation/interpretation monopoly. The victim set is the entire pre-reform literate population — not a subset, but the whole class of agents who could read their own tradition. Suppression is high (0.88) because the constraint required active enforcement: banning Arabic script from public life, dissolving medreses, controlling printing presses, penalizing Ottoman script use. Theater ratio is low (0.22) initially because the enforcement was genuine and brutal; it rises over time as the rupture becomes 'natural' and enforcement shifts from coercion to curriculum. Accessibility collapse is high (0.84) — alternatives (Ottoman script education, private manuscript culture) were legally and materially eliminated. Resistance is significant (0.76) — petitions, underground manuscript preservation, continued private instruction in Arabic script, religious opposition — but was structurally outmatched by state monopoly on education and print. The metrics are authored at six shared time points on one grid (1928, 1935, 1946, 1960, 1980, 2000) so temporal analysis can trace the Snare-to-Piton drift.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (pre-reform literate population, scholarly class, continuity practitioners) experience this as a Snare: active extraction of cultural capital with no consent and no exit. The agenda-setter seat (republican elite) experiences it as a Scaffold with a sunset that never came: 'we needed this rupture to become modern, and once modern we would reconcile with our past' — but the reconciliation never happened, the extraction persisted. The beneficiary seat (post-reform state apparatus) experiences it as a Rope: 'this script coordinates our national unity and technological integration.' The analytical observer seat (contemporary citizens) experiences it as a Mountain: 'Turkish has always been written this way.' The engine computes these four different types from the same structural data — the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-reform state apparatus and republican elite are structural beneficiaries (d ≈ 0.15): they collect the legitimacy rents of 'founding the nation,' control the new educational and bureaucratic field, and face no exit cost — they built the constraint. The pre-reform literate population is the primary target (d ≈ 0.95): they bear the full cost of cultural severance, are identity_locked (their self-concept is constituted through the very tradition the constraint severs), and have no exit — the Ottoman script world is gone. The Ottoman-Islamic scholarly class is even more extremely targeted (d ≈ 0.98): powerless, trapped, their entire institutional basis dissolved. Cultural continuity practitioners sit at d ≈ 0.85: moderate power but identity_locked through their craft. Republican elite have a secondary_role as both agenda_setter and beneficiary — they authored the constraint and collect its rents. Post-1950 opposition politicians are excluded: they would object to the rupture's permanence but cannot enter the conversation without delegitimizing the republic itself. Contemporary citizens are analytical observers: from their seat the constraint appears as a Mountain (script is just 'how Turkish is written'), which is exactly the false_summit_mountain signature the engine should detect if beneficiaries are declared on a Mountain claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (create a modern national identity capable of surviving in a Western-dominated world) was live in 1928. By 1960 it was contested — Turkey was recognized, NATO member, industrializing. By 2000 the founding problem was dead — the nation exists, the script is universal, the rupture is complete. Yet the constraint persists with no sunset clause. The mandate (script as rupture instrument) has atrophied into pure extraction of cultural continuity. The beneficiaries (state apparatus, ideological institutions) maintain the constraint not because the founding problem requires it, but because the rupture is now their legitimacy foundation. This is exactly mandatrophy: the arrangement persists after its function is gone, and the beneficiaries are the ones who would have to authorize its revision. The engine should flag this via founding_problem_status=dead + disappearance_verdict=world_rearranges mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested orthographic_kernel, or an independent constraint?',
    'Structural analysis of whether the beneficiary/victim structure and ε value are stable under this reading alone — if changing to another reading (continuity_reading, modernization_reading) produces a different ε and different victim set, this is a kernel reading.',
    'If kernel reading, the structural delta between readings is the signal; the engine should track the orthographic_kernel family as linked constraints with different ε values. If independent, this story stands alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this story instantiates a kernel reading or an independent constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal enforcement, educational prohibition, state violence) or internalized (population self-censorship, identity fusion with new script, generational amnesia)?',
    'Post-reform trajectory analysis: if suppression metrics persist after formal enforcement relaxes (e.g., after multi-party period 1946+), reclassify as partially internalized. Compare with sibling readings where suppression dynamics differ.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would amplify χ for identity_locked agents and alter the Snare classification durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural rupture').

omega_variable(
    beneficiary_capture_vs_coordination,
    'Does the post-reform state apparatus genuinely benefit from coordination (literacy expansion, technological integration) or is the coordination story cover for ideological capture?',
    'Decompose literacy gains: how much came from script change vs. compulsory education, Latin-script printing infrastructure, and demographic transition? Compare with continuity_reading''s coordination claims (Ottoman script literacy was rising pre-1928).',
    'If coordination gains are separable from rupture, the constraint is a Snare with coordination cover. If inseparable, it approaches Tangled Rope. The engine computes this from beneficiary/victim asymmetry — this omega documents the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_coordination, conceptual, 'Whether coordination function is genuine or cover for extraction').

omega_variable(
    founding_myth_versus_empirical_function,
    'Does the rupture_reading''s foundational axiom (cultural_severance_as_national_birth) remain holdable given empirical outcomes (literacy expansion, Western integration, but also cultural loss)?',
    'Test whether the axiom''s predicted outcomes (clean break enabling modernization) match observed trajectory vs. continuity_reading''s counterfactual (gradual script adaptation preserving textual continuity). The axiom''s status may shift from holdable to overridden within the reading''s own tradition if later Kemalist discourse acknowledges cultural cost.',
    'If the founding axiom is overridden within the reading''s tradition, the constraint''s CS classification shifts — the authority_grounding ''extraction'' gains evidence. If holdable, the rupture narrative remains the legitimate kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_myth_versus_empirical_function, preference, 'Whether the rupture founding myth withstands empirical challenge within its own tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_rupture_tr_t1928, orthographic_kernel__rupture_reading, theater_ratio, 1928, 0.08).
narrative_ontology:measurement(orthographic_rupture_tr_t1935, orthographic_kernel__rupture_reading, theater_ratio, 1935, 0.12).
narrative_ontology:measurement(orthographic_rupture_tr_t1946, orthographic_kernel__rupture_reading, theater_ratio, 1946, 0.18).
narrative_ontology:measurement(orthographic_rupture_tr_t1960, orthographic_kernel__rupture_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(orthographic_rupture_tr_t1980, orthographic_kernel__rupture_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(orthographic_rupture_tr_t2000, orthographic_kernel__rupture_reading, theater_ratio, 2000, 0.22).

% Extraction over time
narrative_ontology:measurement(orthographic_rupture_be_t1928, orthographic_kernel__rupture_reading, base_extractiveness, 1928, 0.95).
narrative_ontology:measurement(orthographic_rupture_be_t1935, orthographic_kernel__rupture_reading, base_extractiveness, 1935, 0.92).
narrative_ontology:measurement(orthographic_rupture_be_t1946, orthographic_kernel__rupture_reading, base_extractiveness, 1946, 0.88).
narrative_ontology:measurement(orthographic_rupture_be_t1960, orthographic_kernel__rupture_reading, base_extractiveness, 1960, 0.82).
narrative_ontology:measurement(orthographic_rupture_be_t1980, orthographic_kernel__rupture_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(orthographic_rupture_be_t2000, orthographic_kernel__rupture_reading, base_extractiveness, 2000, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(orthographic_rupture_su_t1928, orthographic_kernel__rupture_reading, suppression_requirement, 1928, 0.95).
narrative_ontology:measurement(orthographic_rupture_su_t1935, orthographic_kernel__rupture_reading, suppression_requirement, 1935, 0.92).
narrative_ontology:measurement(orthographic_rupture_su_t1946, orthographic_kernel__rupture_reading, suppression_requirement, 1946, 0.85).
narrative_ontology:measurement(orthographic_rupture_su_t1960, orthographic_kernel__rupture_reading, suppression_requirement, 1960, 0.82).
narrative_ontology:measurement(orthographic_rupture_su_t1980, orthographic_kernel__rupture_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(orthographic_rupture_su_t2000, orthographic_kernel__rupture_reading, suppression_requirement, 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% The orthographic_kernel decomposes into three constraint stories with distinct ε values and victim/beneficiary structures. Rupture_reading (this file) has ε=0.92, victims=pre_reform_literate_population+, beneficiaries=post_reform_state_apparatus+, claimed_type=snare. Continuity_reading would have ε≈0.05, victims=[], beneficiaries=[], claimed_type=mountain. Modernization_reading would have ε≈0.45, victims=[religious_traditional_populations], beneficiaries=[state_modernization_apparatus], claimed_type=tangled_rope. The ε-invariance principle requires separate stories because the observable (script change) yields different ε under different readings — they are different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, institutional, 0.15).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, powerless, 0.98).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, organized, 0.95).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, moderate, 0.85).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
