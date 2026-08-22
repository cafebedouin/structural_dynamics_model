% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Script Rupture for National Identity Formation
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   In the early 20th century, the post-Ottoman nation-state mandated
 *   replacement of Arabic script with Latin script for all public writing,
 *   education, and administration. The policy was presented as modernization
 *   and scientific progress; it also functioned as a deliberate rupture
 *   severing connection to the Ottoman Empire and Islamic textual tradition.
 *   This constraint story instantiates the RUPTURE READING of the
 *   orthographic kernel — the reading that emphasizes script change as
 *   intentional erasure of cultural continuity and enforced break with the
 *   pre-reform educated class. This reading sits in contest with two
 *   siblings: the CONTINUITY READING (which frames script change as severing
 *   a living tradition and Islamic knowledge transmission) and the
 *   MODERNIZATION READING (which frames it as pragmatic adoption of a script
 *   suited to scientific/technical communication). The rupture reading's ε is
 *   very high (0.88) because the constraint extracts massive cultural
 *   authority from an entire pre-reform literate population and locks them
 *   into a broken transmission chain where their knowledge becomes
 *   inaccessible and their expertise worthless. The extraction is active and
 *   enforcement-dependent — the state must suppress Arabic-script
 *   publication, exclude pre-reform texts from schools, stigmatize old
 *   literacy as backward. It is a tangled rope because it accomplishes a real
 *   coordination function (unified national script, bureaucratic
 *   standardization) while simultaneously extracting and rupturing.
 *
 * KEY AGENTS:
 *   - Post-reform state apparatus: enforces script change, controls schools and administration, benefits directly from the monopoly on literate legitimacy and the severing of pre-reform authority claims.
 *   - Ottoman educated class: loses economic and cultural value of scribal expertise overnight; identity-locked to pre-reform literacy; suppressed but powerful enough to resist.
 *   - Islamic theological establishment: loses transmission chain for the textual tradition that grounds Islamic law and authority; faces institutional dissolution if it cannot adapt to Latin script; organized but identity-locked.
 *   - Pre-reform literate population: suffers the intergenerational rupture most directly; cannot read new generation's texts; loses cultural capital instantly; suppressed through stigmatization of old literacy as ignorant.
 *   - Post-reform youth: educated exclusively in Latin script; benefit from access to Western technical literature; lose direct access to pre-reform cultural archive.
 *   - Nationalist intellectual class: articulate the ideology that script change enacts; become essential interpreters of modernist identity; powerful beneficiaries.
 *   - Competing regional powers: observe as a model for national consolidation and rupture with imperial/colonial pasts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.88).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.76).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Script Rupture for National Identity Formation").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political/linguistic/cultural").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, 'c3dc2f93-31b4-45cc-b219-dfa181ecd9af').
narrative_ontology:cs_kernel_codification('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', formalized).
narrative_ontology:cs_authority_grounding('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', extraction).
narrative_ontology:cs_interpretation_layer_present('c3dc2f93-31b4-45cc-b219-dfa181ecd9af').
narrative_ontology:cs_reading_relation('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', orthographic_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', foundational, script_change_is_deliberate_rupture).
narrative_ontology:cs_axiom_status(script_change_is_deliberate_rupture, holdable).
narrative_ontology:cs_axiom_grounding('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', script_change_is_deliberate_rupture, instrumental).
narrative_ontology:cs_axiom('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', foundational, national_identity_requires_cultural_discontinuity).
narrative_ontology:cs_axiom_status(national_identity_requires_cultural_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', national_identity_requires_cultural_discontinuity, deontological).
narrative_ontology:cs_reference_frame('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', ottoman_multilingual_textual_order).
narrative_ontology:cs_drift_state('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', contemporary_post_reform_nation_state, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('c3dc2f93-31b4-45cc-b219-dfa181ecd9af', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_educated_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_theological_establishment).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_youth).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, nationalist_intellectual_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, post_reform_youth).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, modernist_break_with_empire).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, national_identity_requires_cultural_rupture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates Latin script adoption via decree, enforces through school curricula, government administration, and publishing controls. Justifies the policy as modernization and scientific progress. Directly controls the script-change mechanism and benefits from the monopoly on literate legitimacy — only post-reform literacy confers cultural and administrative authority. Severs the pre-reform educated class from state positions and makes the old educated class economically obsolete.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Comprises Ottoman scribes, administrators, scholars, and intellectuals whose entire professional and intellectual identity rested on Arabic-script mastery. Overnight, their expertise becomes economically worthless and socially stigmatized. Retraining is possible but psychologically devastating because it requires abandoning the identity that defined them. The old script is the medium through which they think, write, and understand their place in civilization. Exit from the old script means psychological self-erasure. Resistance is immediate but coordinated suppression (bans on Arabic-script publication, exclusion from new-script schools, legal penalties) makes collective action ineffective.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_educated_class, payer,
    powerful, biographical, identity_locked, national).

% The Islamic legal and theological tradition is inscribed in Arabic script — the Quran, hadith, jurisprudential corpus, commentaries. Script change does not erase the texts but breaks the transmission chain: a generation raised in Latin script cannot read the foundational sources without translation or expert mediation. The institutional authority of Islamic law depends on direct textual engagement; mediation through translation corrupts the authority claim. The establishment is identity-locked because Islam itself cannot abandon its scriptural foundation without dissolving. Resistance is framed by the state as backwardness and obscurantism. The establishment is excluded from the script-change decision-making and consulted only after the mandate is issued.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_theological_establishment, payer,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, islamic_theological_establishment, excluded).

% Includes merchants, physicians, poets, administrators, lawyers, scribes — anyone whose social standing and self-concept depended on literacy. The script change erases their cultural capital instantly. They cannot read the new generation's texts; the new generation cannot read theirs. An intergenerational barrier divides them from their children across the same language. The state narrative frames the old script as primitive and the old literacy as ignorant; this stigmatization prevents them from taking pride in their knowledge or transmitting it to their children. Resistance is muted by shame and by the overwhelming institutional suppression (schools teach only the new script, all official documents convert, publishing is controlled).
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    moderate, biographical, identity_locked, national).

% Are educated exclusively in Latin script and cannot read pre-reform texts except through translation or expert mediation. They benefit from access to Western scientific and technical literature (available in Latin script) and internalize the state narrative that Latin script is modern, progressive, and superior to the old script. They also lose direct access to pre-reform cultural production — poetry, literature, history, law, theology. The loss is presented as necessary progress and the price of modernity, so resentment is suppressed and diffused. They gain material access to science but lose cultural depth.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_youth, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_youth, payer).

% Articulates the modernist and nationalist ideology that justifies script change. They position Latin script as the symbol of the new national identity, distinct from and superior to the Ottoman/Islamic past. Script change validates their intellectual framework and makes them essential cultural authorities — they become the interpreters of what is modern, what is national, what is permissible. They monopolize the narrative about script change and benefit professionally and ideologically from the constraint's enforcement. Their exit options are mobile because they can relocate the locus of their intellectualism if challenged, but they profit from the constraint's persistence so they have no incentive to exit.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, nationalist_intellectual_class, beneficiary,
    powerful, generational, mobile, national).

% Observe the script change as a model for national consolidation and cultural rupture. Some emulate it as a tool for severing colonial or imperial pasts; others resist it as cultural imperialism. The constraint's geopolitical resonance carries weight beyond its local function. Their interest is analytical — they assess its effectiveness as a mechanism for state formation and cultural control. They do not directly participate in the constraint's enforcement or benefits.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, competing_regional_powers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified written standard across the post-reform nation-state, enabling bureaucratic administration, school instruction, and national communication without the regional script variants and Ottoman-era fragmentation. Solves the coordination problem of literacy standardization for a post-imperial population.
% TRANSFER_FUNCTION: Transfers cultural and economic authority from the pre-reform educated class (whose expertise is rendered worthless) to the post-reform state apparatus and the nationalist intelligentsia (who monopolize the interpretation of modernity and scientific legitimacy). The transfer also moves the textual authority of Islamic tradition from direct popular literacy into the domain of state-mediated expert interpretation — the state becomes the necessary translator between the population and pre-reform knowledge.
% ABSENT_VOICES: The Ottoman educated class and Islamic theological establishment are the intended target-victims of the constraint but are systematically excluded from the script-change decision. They are consulted only after the mandate is issued and are given no voice in how modernization should proceed. Their absence is deliberate suppression, not accidental omission — the state intentionally excludes them to prevent organized resistance. Alternative modernization paths that would have preserved continuity with pre-reform literacy while adopting new scripts for scientific communication were never considered because they would have preserved pre-reform authority claims.
% DISAPPEARANCE_RATIONALE: If the script change were reversed and Arabic script restored to equal status, the entire institutional and ideological apparatus of the post-reform nation-state would unravel. Education systems would need complete rebuilding; all government records and legal documents would require retranslation; the nationalist identity that was constructed around Latin script as a visible marker of rupture would collapse. The constraint is constitutive of the post-reform state's identity and institutional structure; removing it would require reconstructing the state from foundation.
% FOUNDING_PROBLEM: The Ottoman Empire's decline created a strategic problem for the emerging nation-state: how to establish a new identity visibly distinct from the Islamic imperial past. Adopting a new script made this distinction legible in the material practice of writing — every act of literacy became a statement that the old world had ended. This was deliberate: the state needed to sever the pre-reform educated class's claim to authority and establish the nationalist intelligentsia's monopoly on defining modernity and progress.
% FOUNDING_PROBLEM_CORROBORATION: The post-reform state and nationalist intellectuals attest the founding problem was live and severe. Ottoman historians, pre-reform descendants, and Islamic scholars counter-attest that the problem was manufactured rather than inherited — that the state created the sense of civilizational rupture as a tool of legitimation, not as a necessary response to external crisis. Diaspora Ottoman communities and post-reform scholars from outside the beneficiary set attest that script change was ideological rupture framed as modernization, not a pragmatic choice forced by scientific necessity. Historical comparison with other modernizing states (Japan's script modernization within continuity, China's character simplification) suggests that maintaining script continuity while modernizing is viable — the rupture was chosen, not necessitated.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.88 at interval end) and accumulates over the first 25 years because the constraint's operation grows progressively harsher: initial enforcement is focused on schools and administration; over time, all published texts, signage, and legal documents convert, closing off any retreat or coexistence. The older generation finds their literacy literally useless; the younger generation cannot access the pre-reform archive except through expert mediation. Suppression is sustained at 0.76 because enforcement machinery is required to prevent the old educated class from maintaining Arabic-script publication (bans on printing, confiscation, legal penalties). Theater ratio grows from 0.28 to 0.42 because the state increasingly justifies script change through modernization narratives (access to science, technical progress) rather than through the explicit cultural-rupture framing — the functional justification grows while the extractive mechanism persists. Accessibility collapse is high (0.79) because once the old script is gone from schools and administration, alternatives genuinely collapse; a person born post-reform cannot choose to be literate in the old script without deliberate extra effort. Resistance is substantial (0.68) initially but faces overwhelming structural suppression — the state controls education, publishing, and official communication, making organized collective resistance nearly impossible. The measurement series show extractiveness plateauing after generation 1 (point 25), suggesting the constraint stabilizes once the old literate class ages out and the new generation has no memory of the old script as normal — the extraction accrues entirely to the state and becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The post-reform state apparatus sees the constraint as successful modernization and national unification — a beneficial coordination mechanism. The Ottoman educated class and Islamic establishment see it as violent erasure of their civilization and intentional destruction of their authority. The post-reform youth see it as normal (they know no other script) but may harbor latent resentment or curiosity about the inaccessible past. The nationalist intelligentsia see it as vindicating their ideology and cementing their role as essential mediators. The engine should compute these seats very differently: the agenda-setter and beneficiary seats should show low-to-negative effective extraction; the victim seats (especially ottoman_educated_class at powerful+identity_locked) should show very high effective extraction approaching the maximum. The perspectival gap is exactly the divergence between the constraint's claim (tangled_rope with genuine coordination function) and the victim seats' computed type (snare or severe tangled_rope with extraction dominant).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the post-reform state apparatus: d ≈ 0.05–0.15 (institutional power, high exit options via arbitrage, beneficiary role, controls the constraint). Directionality for ottoman_educated_class: d ≈ 0.85–0.95 (powerful power atom but identity_locked exit, payer role, directly harmed, no way out except psychological abandonment of their own identity). Directionality for islamic_theological_establishment: d ≈ 0.88–0.98 (organized power, identity_locked, payer, civilizational-horizon victim). Directionality for pre_reform_literate_population: d ≈ 0.82–0.92 (moderate power, identity_locked, payer, severe extraction relative to their means). Directionality for post_reform_youth: d ≈ 0.35–0.50 (moderate power, constrained exit, beneficiary+payer dual role, benefits from coordination but loses access to cultural archive). The identity_locked exit option is critical here: it prevents the most harmed parties from moving to alternative scripts or jurisdictions, because their identity is fused with the old literacy itself. Exit means ceasing to be who they are — an infinite cost. This locks them into the target end of the directionality scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman decline, need for new national identity) is live in the moment of policy adoption but becomes contested and then dead within 1–2 generations. By point 20 in the measurement series (roughly 20 years post-adoption), the post-reform generation has no memory of Ottoman rule; the problem the script change was built to solve is no longer salient to them. The state continues enforcing the constraint even though the founding problem is dead — this is a classic mandatrophy trajectory. The theater_ratio rising from 0.28 to 0.42 captures this: the state shifts from framing script change as necessary rupture (explicit connection to the founding problem) toward framing it as modernization and scientific progress (decoupled from the founding problem). The constraint persists as institutional inertia and theater. However, the tangled_rope classification holds because beneficiaries (state apparatus, nationalist intelligentsia) actively profit from the constraint's continued enforcement — they control the gatekeeping of what counts as legitimate modern literacy — so the constraint is not purely inertial. It is a tangled rope that shows signs of mandatrophy drift (theater rising, founding problem dead), but the extraction mechanism remains active because the beneficiary set has structural interest in maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_reading_contest,
    'Is script change a necessary rupture to enable nation-state formation, or a deliberate erasure of a continuous cultural tradition that could have been reformed without breaking?',
    'Comparative historical analysis of nation-states that underwent script change versus those that reformed scripts within continuity (e.g., Japan''s kana/kanji modernization). Compare outcomes on national cohesion, cultural transmission, and resilience of the new identity.',
    'If rupture proves necessary, the constraint''s high ε reflects an authentic structural cost of state formation; if continuity-reform was viable, the high ε reflects manufactured extraction to erase a competing legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuity_reading_contest, conceptual, 'Whether script change was structurally necessary or ideologically chosen.').

omega_variable(
    identity_lock_mechanisms_in_textual_authority,
    'Is the identity-lock exit option accurate for the Ottoman educated class and Islamic establishment, or do they retain exit options (diaspora, underground transmission, hybrid literacies)?',
    'Ethnographic and archival study of post-reform communities: did Ottoman-educated populations maintain Arabic-script literacy in private? Did Islamic institutions preserve textual transmission? Did hybrid literacies emerge allowing code-switching between scripts?',
    'If identity-lock is overstated and exit options persist, effective suppression is lower than authored (0.76) because the constraint does not fully close alternatives. If identity-lock is accurate and transmission breaks, suppression is accurate and the civilizational-horizon harm is severe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanisms_in_textual_authority, empirical, 'Whether pre-reform literacy patterns were truly eliminated or persisted in suppressed form.').

omega_variable(
    modernization_vs_rupture_framing_reading_contest,
    'Is script change primarily a modernization mechanism (adopting Latin script to access Western science) or a rupture mechanism (severing Ottoman/Islamic past as deliberate policy)?',
    'Textual analysis of state policy documents and nationalist intellectual writings: is the justification pragmatic (need for scientific access) or ideological (need to break the past)? Do competing framings coexist in the same texts (modernization_reading versus rupture_reading)?',
    'If primarily modernization, the constraint is more cooperative-seeming and the victim class less clearly intentional; the rupture reading applies, but shares legitimacy with modernization_reading. If primarily rupture with modernization as post-hoc cover story, the constraint''s extractiveness and intentional suppression are more starkly confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modernization_vs_rupture_framing_reading_contest, conceptual, 'Whether script change is framed/justified primarily as modernization or as cultural rupture. Frames the reading-contest between rupture_reading and modernization_reading.').

omega_variable(
    beneficiary_set_temporal_drift,
    'Do the state apparatus and nationalist intelligentsia remain the constraint''s primary beneficiaries over the full interval, or does the beneficiary set shift as the post-reform identity solidifies?',
    'Measure extraction accrual by institutional seat over time: does the state''s revenue from script-monopoly authority decrease after generation 1 (as the new literacy becomes normalized and no longer symbolically defiant)? Do intellectual elites'' gatekeeping power persist?',
    'If beneficiary set shifts, the constraint may transition from tangled_rope (active enforcement, clear beneficiaries) toward piton (inertial persistence, diffuse costs, no concentrated beneficiary). If beneficiary set persists, the tangled_rope classification holds throughout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_set_temporal_drift, empirical, 'Whether the beneficiary set and extraction accrual persist or drift over the constraint''s lifecycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__rupture_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(orth_tr_t5, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__rupture_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t15, orthographic_kernel__rupture_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(orth_tr_t15, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__rupture_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t25, orthographic_kernel__rupture_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(orth_tr_t25, observed).
narrative_ontology:measurement(orth_tr_t30, orthographic_kernel__rupture_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(orth_tr_t30, observed).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__rupture_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(orth_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__rupture_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement_basis(orth_be_t5, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__rupture_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t15, orthographic_kernel__rupture_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement_basis(orth_be_t15, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__rupture_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t25, orthographic_kernel__rupture_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement_basis(orth_be_t25, observed).
narrative_ontology:measurement(orth_be_t30, orthographic_kernel__rupture_reading, base_extractiveness, 30, 0.88).
narrative_ontology:measurement_basis(orth_be_t30, observed).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__rupture_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(orth_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__rupture_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(orth_su_t5, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__rupture_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t15, orthographic_kernel__rupture_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(orth_su_t15, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__rupture_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t25, orthographic_kernel__rupture_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(orth_su_t25, observed).
narrative_ontology:measurement(orth_su_t30, orthographic_kernel__rupture_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(orth_su_t30, observed).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__rupture_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(orth_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% The orthographic kernel contains three structurally distinct constraint stories, each instantiating a different reading of the historical script-change event. rupture_reading (this file) emphasizes deliberate cultural erasure and extraction from the pre-reform literate class. continuity_reading frames script change as severing a living Islamic and Ottoman tradition. modernization_reading frames it as pragmatic adoption of a script suited to scientific communication. All three share the same referent (the historical policy) but differ in ε assessment, beneficiary/victim declaration, and axiom grounding. The three readings coexist in contemporary discourse without logical foreclosure — different parties hold them simultaneously. Each story is linked to the other two via network.affects_constraints to mark them as a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, powerful, 0.91).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
