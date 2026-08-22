% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Conceptual/Institutional Gatekeeping (Became-Thinkable Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint instantiates the 'became-thinkable reading' of the
 *   digital money origin kernel. It holds that digital money emerged as a
 *   historical category when the concept became technically and
 *   institutionally conceivable — not when people first held digital assets
 *   or when regulators formally recognized them, but when the idea entered
 *   the space of legitimate discourse and became an object the institutions
 *   could think about and frame. The constraint is the institutional
 *   gatekeeping that made this thinkability possible and, simultaneously,
 *   restricted whose voice could participate in the thinking. Early
 *   technologists and alternative-currency experimenters had the technical
 *   and theoretical capability to envision digital money, but the
 *   institutional gatekeeping constraint kept their visions from entering the
 *   official narrative about what money is. The constraint's operation
 *   transferred the power to define digital money from decentralized
 *   technical communities to central banking institutions and their allied
 *   theorists. This is a tangled rope: genuine coordination benefit (a shared
 *   definition of money enabled policy coordination and public
 *   understanding), combined with asymmetric extraction (exclusion of
 *   non-institutional voices from the definitional conversation).
 *
 * KEY AGENTS:
 *   - Central banking architects (agenda_setter, institutional): Set the institutional frame for what counts as money; gatekeepers over admission to the conceptual category.
 *   - Financial standards bodies (beneficiary, organized): Consolidated the property-list that defined money; derived authority from exclusion.
 *   - Academic monetary theorists (beneficiary, organized): Provided the intellectual legitimacy for the institutional definition; excluded heterodox framings from academic publication.
 *   - Non-institutional technologists and cypherpunks (payer, powerless): Excluded from the conversation; their alternative visions of digital money were not recognized as monetary theory.
 *   - Alternative-currency experimenters (payer, powerless): Bore the cost of their systems being classified as non-monetary despite performing monetary functions.
 *   - General publics (beneficiary and payer): Benefited from institutional clarity on what money is; did not know they were excluded from alternative possibilities.
 *   - Monetary regulators (observer, institutional): Inherited and enforced the institutional definition; did not create it initially.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.58).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.72).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Conceptual/Institutional Gatekeeping (Became-Thinkable Reading)").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'bef1666b-675f-4432-9b35-58952a03e3e5').
narrative_ontology:cs_kernel_codification('bef1666b-675f-4432-9b35-58952a03e3e5', formalized).
narrative_ontology:cs_authority_grounding('bef1666b-675f-4432-9b35-58952a03e3e5', expertise).
narrative_ontology:cs_interpretation_layer_present('bef1666b-675f-4432-9b35-58952a03e3e5').
narrative_ontology:cs_reading_relation('bef1666b-675f-4432-9b35-58952a03e3e5', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('bef1666b-675f-4432-9b35-58952a03e3e5', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('bef1666b-675f-4432-9b35-58952a03e3e5', foundational, conceptual_thinkability_as_origin).
narrative_ontology:cs_axiom_status(conceptual_thinkability_as_origin, holdable).
narrative_ontology:cs_axiom_grounding('bef1666b-675f-4432-9b35-58952a03e3e5', conceptual_thinkability_as_origin, conventional).
narrative_ontology:cs_axiom('bef1666b-675f-4432-9b35-58952a03e3e5', foundational, institutional_gatekeeping_constitutive).
narrative_ontology:cs_axiom_status(institutional_gatekeeping_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('bef1666b-675f-4432-9b35-58952a03e3e5', institutional_gatekeeping_constitutive, deontological).
narrative_ontology:cs_reference_frame('bef1666b-675f-4432-9b35-58952a03e3e5', institutional_definitional_authority_over_money_category).
narrative_ontology:cs_drift_state('bef1666b-675f-4432-9b35-58952a03e3e5', contemporary_post_blockchain_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bef1666b-675f-4432-9b35-58952a03e3e5', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_banking_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, financial_standards_bodies).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, academic_monetary_theorists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, non_institutional_currency_experimenters).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_technological_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, general_publics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and their academic advisors set the conceptual framework for what counts as money in official discourse. They define the boundary between monetary instruments (credible, tracked, regulated) and non-monetary digital systems (speculative, untracked, excluded). They benefit from gatekeeping authority over the definition: it protects established money's legitimacy and regulatory reach. Their framework shaped what technologists could imagine as money and what regulatory bodies would recognize.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_banking_architects, agenda_setter,
    institutional, generational, analytical, global).

% Bodies like the Basel Committee, BIS, and national standard-setting authorities consolidated the property-list that defined money (medium of exchange, store of value, unit of account, issued by recognized authority, traceable). This list became the gate: systems outside the list were not money, regardless of function. Their benefit was institutional authority and the ability to exclude rival framings.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, financial_standards_bodies, beneficiary,
    organized, generational, arbitrage, global).

% Economists teaching Keynesian and neoclassical monetary theory had a vested intellectual position in the state-issued, centrally-managed money framework. Digital money systems that challenged this model faced dismissal from mainstream academic outlets. Their benefit was epistemological authority: the academy could declare what was theoretically coherent and what was not.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, academic_monetary_theorists, beneficiary,
    organized, generational, constrained, global).

% Technologists, cypherpunks, and alternative-currency communities (LETS systems, time banks, peer-to-peer exchange networks) attempted to build and theorize digital money outside the institutional gate. They bore the cost of exclusion: their systems were not recognized as money, regulatory agencies treated them as gambling or securities fraud, mainstream funding was unavailable, and they were locked out of the conceptual apparatus itself — they could not publish in official outlets or influence policy.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, non_institutional_currency_experimenters, payer,
    powerless, immediate, trapped, local).

% Historical technological traditions for value transfer that did not fit the institutional definition (barter networks, mutual credit systems, value databases not under central control) were systematically reframed as pre-monetary or non-monetary by the official narrative. They could not enter the conversation about what digital money was because the language itself excluded them. Those working in these traditions bore the epistemic cost of permanent subordination.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_technological_traditions, payer,
    powerless, biographical, identity_locked, global).

% National and supranational regulators (SEC, CFTC, FinCEN, central bank governors) observed the gatekeeping and eventually aligned themselves with the institutional definition. They relied on the conceptual boundary to distinguish monetary instruments (their domain) from unregulated systems. They did not create the boundary initially but inherited and enforced it.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_regulators, observer,
    institutional, generational, analytical, national).

% Technologists and researchers exploring digital currency architectures (David Chaum, the Cypherpunks mailing list, early e-cash researchers) were excluded from mainstream institutional conversation. Their work was not treated as monetary theory; it was treated as cryptography or computer science, epistemically separated from money. They would have challenged the gatekeeping definition but were never seated at the table.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_digital_money_technologists, excluded,
    moderate, biographical, constrained, global).

% Ordinary people benefited from the institutional clarity that the gatekeeping imposed: they knew what money was, could trust it because it was regulated, and did not have to evaluate competing claims about what counted as a store of value. This was genuine coordination benefit. The cost was that alternative visions of money — decentralized, privacy-respecting, non-state — were never available to them as options, and they did not know they were excluded from the conversation about what digital money should be.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, general_publics, beneficiary,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, central_banking_architects).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a canonical, institution-validated definition of money as a category: defining which properties (state issuance, traceability, regulatory authority, unit-of-account status) mark something as money versus non-money. This allowed policymakers, technologists, and the public to coordinate around a shared understanding of what could count as money and what could not. The definition made the problem of monetary authority coherent.
% TRANSFER_FUNCTION: Moves epistemic authority over the money concept from decentralized technical and economic communities toward central banking institutions and their theoretical allies. Early technologists and alternative-currency experimenters surrender their ability to define money; institutional authorities gain the power to admit or exclude new systems from the category. The constraint also transfers legitimacy: systems the institutions call money are credible; systems they exclude are speculative, dangerous, or non-monetary.
% ABSENT_VOICES: Non-institutional technologists, cypherpunks, alternative-currency community leaders, and heterodox economists who theorized money outside the neoclassical/Keynesian mainstream were systematically excluded from the conceptual conversation. They would have argued for decentralized, privacy-preserving, or community-issued alternatives, but they were not seated in the institutions where the definition was established. Academic journal editors, central bank advisory boards, and regulatory standard-setting bodies controlled who got to speak.
% DISAPPEARANCE_RATIONALE: If the institutional gatekeeping on the money definition vanished, the conceptual landscape would rearrange significantly. Alternative frameworks for digital money (decentralized consensus, community-issued, privacy-native, asset-backed but not state-issued) would immediately become thinkable and publishable. Technologists and experimenters currently excluded would re-enter the conversation. However, some parties would argue the definition was never a gate but a description of what money actually is, so removing it would just let confusion in, not liberation.
% FOUNDING_PROBLEM: In the early postwar period, central banking institutions and academic monetary theory needed a shared, transmissible definition of money to coordinate policy across jurisdictions and to distinguish legitimate monetary functions from other financial activities (gambling, securities, barter). Digital technologies raised the question sharply: what properties would a digital thing need to possess to count as money? The gate was built to answer this question institutionally rather than letting it be answered by each technology independently.
% FOUNDING_PROBLEM_CORROBORATION: Central banking institutions and mainstream monetary economists attest the founding problem is still live: they continue to maintain gatekeeping definitions and argue they are necessary for monetary stability and policy transmission. Heterodox economists, blockchain researchers, and alternative-currency theorists attest the founding problem is obsolete and the gate is now rent protection: they argue digital money systems can coordinate on their own, that the institutional definition excludes legitimate innovations, and that the gate persists only because institutions benefit from controlling the category. Published critiques from outside the benefiting parties (academic dissidents, technologists, developing-nation monetary reformers) support the rent-protection reading.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint transfers epistemic and institutional authority but also provides genuine coordination benefits. Early in the interval (t=0, ε=0.35), the gate was permeable and contested — technologists and theorists could still publish alternatives and the outcome was uncertain. Over the interval, extractiveness rises (t=40, ε=0.60) as the institutional definition solidifies and competing framings are progressively excluded from legitimate discourse. Suppression is higher (0.72 at t=25) because enforcement is not merely passive filtering but active exclusion: regulatory agencies classify non-institutional systems as securities or gambling, academic journals reject papers on alternative money concepts, and funding dries up for research outside the institutional frame. Theater ratio rises gradually (from 0.25 to 0.44), reflecting that as the gate becomes entrenched, more enforcement activity is devoted to defending the institutional definition itself rather than its original coordination purpose. Accessibility collapse is moderate (0.48) because alternatives remain theoretically conceivable (mathematically, technically, economically) — they are not physically impossible, only institutionally illegitimate. Resistance is substantial (0.67) because technologists and theorists continuously challenge the gate, though from positions of institutional weakness.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between institutional and non-institutional seats. Institutions that benefit from the gatekeeping (central banks, academic establishments) perceive the constraint as legitimate coordination apparatus — the definition is not arbitrary but descriptive of what money really is. Non-institutional technologists perceive the constraint as arbitrary power: they can point to systems their definition would classify as money, systems the institutional definition excludes, and ask why the institutional frame gets to be the canonical one. Neither perception is false; they are seated differently relative to the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional asymmetry comes from exit options and beneficiary status. Central banking architects have analytical-level exit (they set the frame, so they always have the choice to change it) and they are the exclusive beneficiaries (the gate protects their institutional prerogative). Non-institutional technologists have trapped exit (their identity and research career are fused to digital money exploration; abandoning the research means abandoning their vocational identity — this is identity_locked exit). This structural asymmetry maps directly to directionality: high-exit beneficiaries get d near 0.0; low-exit, identity-locked targets get d near 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: in the postwar period, monetary institutions needed a canonical definition of money to coordinate policy and regulate financial stability. The gate was built to solve that problem. However, as digital technologies advanced and non-institutional experiments proliferated, the gate's function shifted: it became less about coordination and more about protecting institutional prerogative over the money category. By t=25-30, the measurement data show rising theater_ratio (0.36-0.41) suggesting increasing performativity: enforcement activity is devoted less to solving the coordination problem (which is largely solved — everyone knows what money is) and more to defending the gate itself (excluding competing definitions). The founding_problem_status=contested reflects this: some parties attest the problem is still live (institutions need clear definitions to regulate); others attest it is dead (digital money coordinates itself, the gate is now just rent extraction). The constraint remains structurally tangled_rope (both coordination and extraction are real), but the diagnosis from mandatrophy analysis suggests the coordination-to-extraction ratio is deteriorating over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_legitimacy_ambiguity,
    'Is the institutional definition of money a genuine description of what money is, or is it an arbitrary imposition of power that excludes legitimate alternatives?',
    'Cross-cultural and historical comparison: do all human societies use state-issued, centrally-managed, traceable instruments as money, or do diverse monetary systems flourish under different definitions? If diverse systems are stable without institutional gatekeeping, the gate is power-imposition; if they converge on state-issued instruments, the gate is descriptive.',
    'If the definition is genuine/descriptive, the constraint approaches rope (coordination benefit is primary, gate is necessary). If the definition is arbitrary/imposed, the constraint is snare (extraction is primary, gate is rent-seeking). This impacts the type computation for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_legitimacy_ambiguity, conceptual, 'Whether institutional monetary definitions are natural/descriptive or constructed/extractive.').

omega_variable(
    excluded_voice_capability_gap,
    'Did the excluded technologists and alternative-currency experimenters possess genuine monetary insights that the institutional gate prevented from being heard, or were they pursuing incoherent or unworkable ideas?',
    'Historical review of excluded technical work (cypherpunk archives, early digital cash research, mutual-credit systems) by neutral parties: did the ideas anticipate features later implemented successfully (decentralized consensus, privacy-preserving transactions, non-state issuance)? Or were they technically naive?',
    'If excluded voices had genuine insights, the suppression was costly to society and the gate''s beneficiaries captured value that should have been widely distributed. If the ideas were incoherent, suppression was protective. This affects whether the constraint is primarily extractive (victims had something to say) or primarily protective (victims were trying to harm coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_voice_capability_gap, empirical, 'Whether excluded communities possessed legitimate monetary innovations or pursued unworkable ideas.').

omega_variable(
    kernel_reading_dominance,
    'Does the origin of digital money fall primarily at the became-thinkable moment (this reading), at first widespread holding (first_held_reading), or at institutional recognition (regulatory_recognition_reading)?',
    'This is a framing question, not empirically resolvable in the traditional sense. The resolution is whether the scientific and policy communities converge on which moment best captures the historical significance of digital money''s emergence. Convergence would not make one answer true; it would just establish which framing is operationally canonical.',
    'If became-thinkable dominates, this constraint story defines the origin and frames the subsequent history. If first_held or regulatory_recognition dominate, this story becomes a prehistory: the gatekeeping that made digital money thinkable but the actual origin comes later. The dominance question is not about which reading is true but about which reading the institutions and historians adopt as the canonical one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dominance, preference, 'Which moment in the digital-money timeline is treated as the origin: conceptual possibility, first practical holding, or regulatory recognition.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (external barriers like regulatory exclusion and journal rejection) or internalized (excluded technologists have absorbed the belief that their work is not monetary theory)?',
    'Post-gate removal test: if the institutional barriers were removed (journals opened, regulators neutral, central banks funded research), would excluded communities immediately re-enter and propose alternatives, or have they been so marginalized that they no longer try? Persistence of exclusion after barrier removal indicates internalization.',
    'If suppression is structural, removing the gate would liberate excluded voices and alter the constraint''s type dramatically. If suppression is internalized, removing barriers alone would not restore participation — the victims would need active restoration of credibility and voice. This affects the fixing_cost estimate and the trajectories after gate removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is external (barriers) or internalized (victim belief in delegitimation).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(digi_tr_t0, observed).
narrative_ontology:measurement(digi_tr_t5, digital_money_origin__became_thinkable_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(digi_tr_t5, observed).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__became_thinkable_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(digi_tr_t10, observed).
narrative_ontology:measurement(digi_tr_t15, digital_money_origin__became_thinkable_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(digi_tr_t15, observed).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__became_thinkable_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(digi_tr_t20, observed).
narrative_ontology:measurement(digi_tr_t25, digital_money_origin__became_thinkable_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(digi_tr_t25, observed).
narrative_ontology:measurement(digi_tr_t30, digital_money_origin__became_thinkable_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(digi_tr_t30, projected).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__became_thinkable_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(digi_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(digi_be_t0, observed).
narrative_ontology:measurement(digi_be_t5, digital_money_origin__became_thinkable_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(digi_be_t5, observed).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__became_thinkable_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(digi_be_t10, observed).
narrative_ontology:measurement(digi_be_t15, digital_money_origin__became_thinkable_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(digi_be_t15, observed).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__became_thinkable_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(digi_be_t20, observed).
narrative_ontology:measurement(digi_be_t25, digital_money_origin__became_thinkable_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(digi_be_t25, observed).
narrative_ontology:measurement(digi_be_t30, digital_money_origin__became_thinkable_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(digi_be_t30, projected).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__became_thinkable_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(digi_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(digi_su_t0, observed).
narrative_ontology:measurement(digi_su_t5, digital_money_origin__became_thinkable_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(digi_su_t5, observed).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__became_thinkable_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(digi_su_t10, observed).
narrative_ontology:measurement(digi_su_t15, digital_money_origin__became_thinkable_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(digi_su_t15, observed).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__became_thinkable_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(digi_su_t20, observed).
narrative_ontology:measurement(digi_su_t25, digital_money_origin__became_thinkable_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(digi_su_t25, observed).
narrative_ontology:measurement(digi_su_t30, digital_money_origin__became_thinkable_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(digi_su_t30, projected).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__became_thinkable_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(digi_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'digital_money_origin' kernel. The became-thinkable reading focuses on institutional gatekeeping that precedes implementation. The first_held reading identifies the origin at practical adoption (post-Bitcoin, post-mobile-payment). The regulatory_recognition reading identifies the origin at formal institutional incorporation. Each reading has a different ε, different beneficiary/victim sets, and covers a different part of the historical timeline. They are linked via affects_constraints because the institutional gatekeeping in the became-thinkable reading creates downstream pressure on the first_held reading (if digital money is already thinkable and framed by institutions, implementers face that framing as a constraint) and the regulatory_recognition reading (formal recognition builds on prior conceptual possibility). The three readings together form a constraint family covering the full origin question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
