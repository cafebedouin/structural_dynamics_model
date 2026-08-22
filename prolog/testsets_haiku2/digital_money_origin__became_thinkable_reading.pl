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
 *   human_readable: Conceptual Gatekeeping: Digital Money Origin via Institutional Thinkability
 *   domain: monetary_history/institutional_economics/technology_studies
 *
 * SUMMARY:
 *   This reading instantiates the 'became_thinkable' origin narrative:
 *   digital money emerges at the moment when institutional actors (central
 *   banks, financial regulators, payment processors) develop a conceptual
 *   framework through which non-physical monetary instruments become
 *   recognizable and manageable as 'money.' The origin is not the first
 *   technical prototype (which preceded thinkability) nor the first
 *   regulatory incorporation (which followed it), but the moment when the
 *   concept itself became institutionally intelligible. The constraint
 *   operates as a tangled rope: it coordinates a genuine problem (how to make
 *   the technical frontier legible to policy systems) while simultaneously
 *   extracting definitional power (institutional actors monopolize the
 *   authority to say what 'counts' as digital money). Beneficiaries are the
 *   institutional architects whose conceptual frame becomes canonical;
 *   victims are alternative theorists and non-institutional actors whose
 *   concepts are rendered invisible or illegitimate by the same mechanism.
 *
 * KEY AGENTS:
 *   - institutional_architects: Agenda-setters who define thinkability (power:institutional, exit:arbitrage) — benefit from monopoly on conceptual framing
 *   - central_banks: Beneficiaries who gatekeep incorporation into policy (power:institutional, exit:arbitrage) — absorb innovations on their terms
 *   - early_financial_technologists: Beneficiaries within the institutional framework (power:organized, exit:constrained) — funded and legitimated only when aligned with institutional concepts
 *   - alternative_monetary_theorists: Victims rendered invisible (power:moderate, exit:identity_locked) — bear the cost of working outside recognized frameworks
 *   - grassroots_payment_communities: Victims trapped outside institutional visibility (power:powerless, exit:trapped) — cannot scale without institutional recognition
 *   - alternative_payment_technologists: Excluded from the thinkability conversation (power:organized, exit:constrained) — their work deemed 'outside the domain' by definitional fiat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.71).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Conceptual Gatekeeping: Digital Money Origin via Institutional Thinkability").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/institutional_economics/technology_studies").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'f25daeeb-6f56-4040-ab7c-1090c0ddd7e9').
narrative_ontology:cs_kernel_codification('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', distributed).
narrative_ontology:cs_authority_grounding('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', distributed).
narrative_ontology:cs_reading_relation('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', foundational, institutional_thinkability_as_emergence_marker).
narrative_ontology:cs_axiom_status(institutional_thinkability_as_emergence_marker, holdable).
narrative_ontology:cs_axiom_grounding('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', institutional_thinkability_as_emergence_marker, conventional).
narrative_ontology:cs_axiom('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', secondary, monetary_legitimacy_requires_institutional_recognition).
narrative_ontology:cs_axiom_status(monetary_legitimacy_requires_institutional_recognition, holdable).
narrative_ontology:cs_axiom_grounding('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', monetary_legitimacy_requires_institutional_recognition, deontological).
narrative_ontology:cs_reference_frame('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', institutional_monetary_coherence).
narrative_ontology:cs_drift_state('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', contemporary_decentralized_finance_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('f25daeeb-6f56-4040-ab7c-1090c0ddd7e9', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_financial_technologists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, alternative_monetary_theorists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, grassroots_payment_communities).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, non_institutional_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks, finance ministries, and established payment processors set the conceptual framework through which 'digital money' becomes intelligible. They decide what technical properties and institutional recognitions suffice for a thing to count as money. This framing excludes peer-to-peer systems and non-state-backed instruments from the conversation about monetary emergence. They benefit by maintaining definitional control over what counts as legitimate monetary innovation.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, institutional_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the institutional thinkability framework by gatekeeping which digital instruments can be incorporated into money aggregates (M1, M2, etc.) and regulatory oversight. Their monopoly on the concept of monetary legitimacy allows them to absorb innovations into state frameworks on their terms. They do not directly implement digital money but adjudicate its conceptual status.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_banks, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, central_banks, agenda_setter).

% Technologists employed by banks or working within regulatory frameworks benefit because institutional recognition of their concepts as 'digital money' grants them access to capital, regulatory approval, and legitimacy. Their innovations are legible and fundable only when they align with institutional thinkability constraints. Those who work within the framework are elevated; those who resist it are marginalized.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_financial_technologists, beneficiary,
    organized, biographical, constrained, global).

% Theorists of alternative money systems (mutual credit, commodity-backed systems, purely peer-to-peer designs) bear the cost of operating outside the institutional thinkability framework. Their concepts are deemed 'not really money' by the dominant framework, which suppresses research funding, institutional partnership, and policy consideration. Their exit from this constraint would require abandoning the quest for institutional legitimacy—a high identity cost for actors who see reform as their mandate.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, alternative_monetary_theorists, payer,
    moderate, biographical, identity_locked, global).

% Communities experimenting with local digital currencies, time banks, or alternative payment systems find their innovations unrecognized and unsupported by the institutional thinkability framework. They cannot access the capital or regulatory pathways available to centrally-recognized digital money. Their exclusion from 'what counts as money' constrains their ability to scale and persist.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, grassroots_payment_communities, payer,
    powerless, biographical, trapped, local).

% Individuals and informal networks attempting to create money-like instruments without institutional backing are structurally excluded from the thinkability framework. They cannot influence which concepts are deemed 'digital money' because the definitional power sits entirely with institutional actors. Any innovation they produce is evaluated retroactively by the institutional framework and accepted or rejected on those terms.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, non_institutional_actors, payer,
    powerless, immediate, trapped, local).

% International bodies like the IMF and BIS observe and record how digital money becomes institutionally thinkable. They analyze the boundary between technology (technical feasibility of non-physical payment) and institution (political acceptance into regulatory regimes). Their role is analytical: they measure the constraint but do not directly author it.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_authorities, observer,
    institutional, generational, analytical, global).

% Technologists building peer-to-peer, decentralized, or non-state-backed digital payment systems are structurally excluded from the institutional thinkability conversation. Their work is deemed outside the domain of 'digital money' by definitional fiat—relegated to 'technology,' 'speculation,' or 'experiment' rather than legitimate monetary innovation. They would object to the constraint's definition of what counts as money if they had a seat at the table, but the constraint structures their exclusion.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, alternative_payment_technologists, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, institutional_architects).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common institutional language for recognizing digital monetary instruments: what technical and organizational properties must a system possess for a monetary authority to incorporate it into policy, statistical measurement, and regulatory frameworks. This solves the problem of coordination among disparate actors around a shifting technological frontier: without shared concepts, regulators and technologists cannot communicate about which innovations 'count.'
% TRANSFER_FUNCTION: Transfers definitional power (the authority to say what is and is not 'digital money') from potential users and inventors to established institutional actors (central banks, finance ministries, payment processors). This movement of authority enables institutional gatekeeping: technologies that fit the thinkability frame attract capital and legitimacy; those that do not are starved of resources and institutional support. The beneficiaries gain the power to adjudicate emergence; the excluded bear the cost of invisibility.
% ABSENT_VOICES: Alternative monetary theorists, grassroots payment communities, and decentralized-finance architects who reject the institutional thinkability frame are structurally absent from the conversation. They would argue that 'digital money' includes any system that functions as money (store of value, medium of exchange, unit of account) regardless of institutional backing, expanding the domain far beyond what central banks recognize. Their exclusion is deliberate: the constraint structures their non-participation.
% DISAPPEARANCE_RATIONALE: If the institutional gatekeeping constraint on thinkability vanished, the conceptual domain of 'digital money' would expand immediately. Peer-to-peer systems, commodity-backed instruments, and community-scale alternatives would become thinkable and discussable as legitimate monetary forms. Investment, research, and regulatory attention would shift. The institutional actors' monopoly on defining emergence would dissolve, and multiple origin narratives (first-held, regulatory-recognized, and others) would compete in public discourse without institutional suppression.
% FOUNDING_PROBLEM: In the 1970s–1990s, the technical frontier of non-physical payment systems advanced faster than institutional conceptual frameworks could absorb. Banks and payment processors built electronic funds transfer; technologists built cryptographic systems; but there was no stable institutional consensus on what counted as 'money' in this new domain. The problem: how to make the new technical possibilities legible and manageable to regulatory systems that had evolved around physical cash and documented accounts?
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians (Rogoff, Rogoff & Stein, Birger on payment systems), central bank research divisions (BIS publications on digital currencies), and independent technology historians (Maurer on payment systems, Swanson on the history of digital money concepts) all corroborate that institutional thinkability was a genuine coordination problem in the 1980s–2000s. They document central banks' struggle to incorporate electronic money into M2 aggregates, the regulatory uncertainty around e-money systems, and the deliberate framing choices that institutional actors made. No corroborating source from outside the institutional framework is named: alternative monetary theorists and grassroots communities do not have comparable published historical records—a fact the constraint itself explains.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.35 (early 1970s, when concepts were still plural and contested) to a plateau of 0.62 (by 2010s, when institutional consensus solidified). The rise tracks the consolidation of institutional gatekeeping power: as central banks coordinated on what 'counts,' they simultaneously excluded competing frameworks. Suppression rises from 0.55 to 0.71 over the same interval, reflecting active enforcement: exclusion from research funding, regulatory non-recognition of alternative money concepts, and deliberate redefinition of terms to map onto institutional categories only. Theater ratio rises from 0.25 to 0.41, indicating that much of the institutional activity around 'digital money' is spent defending and updating the definitional boundary rather than solving the original coordination problem (which was largely solved by the 1990s). Accessibility collapse remains low-to-moderate (0.48) because the excluded actors retain the option of working outside institutional frameworks; they cannot be entirely suppressed, only rendered invisible to institutional power and capital flows. Resistance is moderate (0.58) because alternative theorists and grassroots communities mount continuous conceptual challenges, even without institutional platforms. The shared time grid aligns all three metrics: every time point is authored for every tracked metric, reflecting a single trajectory of institutional consolidation.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional architect seat, 'what counts as digital money' is a technical and regulatory question to be solved by careful definitional work—a coordination function. From the alternative theorist seat, the same definitional work is an act of gatekeeping and delegitimation—a mechanism of exclusion. The constraint's persistence depends on the institutional seat's perception remaining the authoritative one: if the alternative seat's perception becomes equally credible, the constraint's legitimacy collapses. The engine computes these divergent types from the structural data (beneficiary vs. victim, arbitrage vs. identity-locked exit) without needing to reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional architects and central banks sit at the beneficiary end of directionality (d ≈ 0.1–0.2): the constraint amplifies their authority and their ability to absorb innovations on their terms. Early financial technologists sit near the low-extraction end (d ≈ 0.3–0.4) because they benefit from institutional legitimacy while also bearing modest costs (constrained innovation paths, regulatory compliance). Alternative theorists and grassroots communities sit at the high-extraction end (d ≈ 0.8–0.9): they bear the full cost of exclusion from capital, recognition, and policy influence. Monetary authorities sit at the analytical observation point (d ≈ 0.5, symmetric): they study the constraint without benefiting or suffering from it directly. The engine should compute seat-specific types: the central bank seat experiences this as rope (genuine coordination, legitimate function); the alternative theorist seat experiences it as snare (extraction disguised as coordination).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: in the 1970s–1990s, institutional actors genuinely needed a shared framework for recognizing and incorporating new payment technologies. That problem was substantially solved by 2010: the criteria for incorporating electronic payment systems into money aggregates were established, regulatory pathways were defined, and central banks had developed coherent concepts. Yet the constraint persists and even intensifies (theater_ratio rises, suggesting performative activity replacing functional activity). Mandatrophy: the institutional gatekeeping function outlived its founding problem. What remains is the extraction (institutional control over definitional power) divorced from the coordination (the technical problem was solved long ago). This is exactly the pattern a piton would show, except the constraint is still actively enforced (suppression remains high), so it is more accurately described as a tangled_rope transitioning toward piton-like dynamics—coordination function atrophied, extraction mechanism intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concept_precedence_vs_practice,
    'Is institutional conceptualization of digital money a legitimate marker of ''emergence,'' or merely an intellectual precondition that should not be counted as emergence itself?',
    'Philosophical analysis of what constitutes ''emergence'' of a technology: does it require conceptual coherence, first implementation, first adoption, or regulatory recognition? Different frameworks yield different origin dates for the same constraint object.',
    'If conceptualization alone counts as emergence, this reading wins the contest (earliest origin). If emergence requires adoption or recognition, sibling readings are more defensible. The divergence is conceptual/definitional, not empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(concept_precedence_vs_practice, conceptual, 'Philosophical status of conceptual thinkability as an emergence marker').

omega_variable(
    gatekeeping_function_vs_extraction,
    'Is the institutional thinkability constraint primarily a coordination mechanism solving a genuine technical-policy problem, or primarily an extraction mechanism through which institutional actors monopolize definitional power?',
    'Counterfactual analysis: if alternative monetary theorists had been admitted to the thinkability conversation, would the resulting concepts have been substantively different? If yes, the constraint is extractive gatekeeping; if no, it is genuine coordination.',
    'If extractive, the classification shifts from tangled_rope (both coordination and extraction) to snare (extraction disguised as coordination). If coordination-dominant, the tangled_rope classification stands. The answer determines how much of the measured extraction is inherent to the coordination function and how much is pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_function_vs_extraction, empirical, 'Whether gatekeeping serves coordination or captures institutional power').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.71) primarily structural (institutional actors actively deny resources and platforms to excluded groups) or internalized (excluded actors internalize the judgment that their concepts ''don''t count'' and stop attempting institutional engagement)?',
    'Post-suppression trajectory analysis: if alternative monetary communities were suddenly granted institutional platforms and funding, would they reengage institutional thinkability conversations, or would their identities be too formed around outsider positioning to accept institutional recognition?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the excluded carry the suppression with them even if external barriers dissolve. If structural, the suppression would drop immediately upon barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized mechanisms of suppression').

omega_variable(
    sibling_reading_foreclosure,
    'Does institutional thinkability as an origin marker logically foreclose the ''first_held_reading'' (individual adoption as emergence), or can both readings coexist as valid markers of different aspects of emergence?',
    'Logical analysis: can a system be said to have ''emerged'' both when it became institutionally thinkable AND when it was first practically held by individuals? Or are these mutually exclusive origin claims?',
    'If mutually exclusive (forecloses relation), only one sibling can be true. If both can hold simultaneously (coexists_with), the readings are complementary rather than competitive. Affects the modal-logic interpretation of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between thinkability and adoption as competing emergence markers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(digi_tr_t0, projected).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__became_thinkable_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(digi_tr_t8, projected).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__became_thinkable_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(digi_tr_t16, observed).
narrative_ontology:measurement(digi_tr_t25, digital_money_origin__became_thinkable_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(digi_tr_t25, observed).
narrative_ontology:measurement(digi_tr_t35, digital_money_origin__became_thinkable_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(digi_tr_t35, observed).
narrative_ontology:measurement(digi_tr_t50, digital_money_origin__became_thinkable_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(digi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(digi_be_t0, projected).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__became_thinkable_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(digi_be_t8, projected).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__became_thinkable_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement_basis(digi_be_t16, observed).
narrative_ontology:measurement(digi_be_t25, digital_money_origin__became_thinkable_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement_basis(digi_be_t25, observed).
narrative_ontology:measurement(digi_be_t35, digital_money_origin__became_thinkable_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(digi_be_t35, observed).
narrative_ontology:measurement(digi_be_t50, digital_money_origin__became_thinkable_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(digi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(digi_su_t0, projected).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__became_thinkable_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(digi_su_t8, projected).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__became_thinkable_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(digi_su_t16, observed).
narrative_ontology:measurement(digi_su_t25, digital_money_origin__became_thinkable_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(digi_su_t25, observed).
narrative_ontology:measurement(digi_su_t35, digital_money_origin__became_thinkable_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(digi_su_t35, observed).
narrative_ontology:measurement(digi_su_t50, digital_money_origin__became_thinkable_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(digi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel decomposes into three constraint stories, each representing a competing reading of when digital money 'emerged.' Each reading has a different origin date, different beneficiary/victim structure, and different ε value. This reading (became_thinkable) dates emergence earliest and emphasizes institutional conceptualization as the emergence marker. The sibling readings shift the marker to first adoption (first_held) and formal regulatory incorporation (regulatory_recognition). All three are linked by network.affects_constraints: institutional thinkability constrains which adoption narratives become thinkable, and adoption constrains which regulatory incorporation frameworks become necessary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__became_thinkable_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
