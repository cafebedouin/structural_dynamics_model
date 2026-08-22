% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary — Theoretical Conceptualization Reading
 *   domain: monetary economics/financial history/technology governance
 *
 * SUMMARY:
 *   This story instantiates the conceptualization_reading of the
 *   digital_money_emergence_boundary kernel: the periodization convention
 *   that dates digital money's emergence to theoretical thinkability — the
 *   1960s telecommunications advances that made electronic value transfer
 *   first designable, and Chaum's 1985 blind-signature formalization that
 *   made cryptographic cash first specifiable. The convention solves a real
 *   coordination problem (a scattered history spanning telecom engineering,
 *   banking operations, and cryptography needed a citable origin) while
 *   allocating credit asymmetrically: the academic/research community
 *   collects priority claims, citation rents, and the founding title;
 *   infrastructure-era engineers are retroactively demoted to precursor
 *   status; adoption-era historians lose narrative priority; monetary
 *   statisticians inherit a potential-money classification burden in the
 *   aggregates; and independent emergences outside the lineage, such as
 *   telecom-airtime money, are rendered invisible. The epsilon referent is
 *   the standing arrangement under contest — the thinkability-boundary
 *   convention as it operates — assessed by this reading's own lights: the
 *   reading endorses the boundary as the descriptively correct emergence
 *   point, which is why epsilon is moderate rather than high even though the
 *   structural data plainly records who pays for the convention's
 *   maintenance. The claimed type and the metrics are authored independently:
 *   tangled_rope is claimed from the structure (genuine coordination plus
 *   asymmetric credit allocation plus active gatekeeping), while the metrics
 *   describe the convention's observed operation. The sibling readings are
 *   separate constraints, not described or averaged into this one.
 *
 * KEY AGENTS:
 *   - academic_research_community: primary beneficiary and agenda-setter (organized / identity_locked) — collects priority claims and administers the gatekeeping that holds the 1985 boundary canonical
 *   - infrastructure_era_engineers: primary target (organized / identity_locked) — the 1967–77 ATM/ACH/SWIFT builders, retroactively demoted to precursor status; their professional identity is fused with the founding title the convention assigns elsewhere
 *   - monetary_statisticians: secondary target (institutional / constrained) — central-bank and BIS statistical divisions bearing the potential-money classification burden in the aggregates
 *   - economic_historians_of_adoption: contesting target (moderate / mobile) — publish the rival periodizations; their mobility damps what they bear
 *   - digital_cash_venture_founders: secondary beneficiary (moderate / constrained) — converted the priority date into patents and venture legitimacy; personally monetized the claims even as the ventures failed
 *   - mobile_money_operators: excluded voice (powerful / constrained) — telecom-airtime money emergences with no lineage to the canonical origin; rendered invisible by the single-boundary convention
 *   - central_bank_archivists: analytical observer (institutional / analytical) — hold the full documentary record across all three readings' periods; the common evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.44).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.48).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary — Theoretical Conceptualization Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary economics/financial history/technology governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '58997cb6-1c10-4ed2-8560-57d7b8798375').
narrative_ontology:cs_kernel_codification('58997cb6-1c10-4ed2-8560-57d7b8798375', distributed).
narrative_ontology:cs_authority_grounding('58997cb6-1c10-4ed2-8560-57d7b8798375', expertise).
narrative_ontology:cs_interpretation_layer_present('58997cb6-1c10-4ed2-8560-57d7b8798375').
narrative_ontology:cs_reading_relation('58997cb6-1c10-4ed2-8560-57d7b8798375', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('58997cb6-1c10-4ed2-8560-57d7b8798375', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('58997cb6-1c10-4ed2-8560-57d7b8798375', foundational, emergence_equals_theoretical_thinkability).
narrative_ontology:cs_axiom_status(emergence_equals_theoretical_thinkability, holdable).
narrative_ontology:cs_axiom_grounding('58997cb6-1c10-4ed2-8560-57d7b8798375', emergence_equals_theoretical_thinkability, conventional).
narrative_ontology:cs_axiom('58997cb6-1c10-4ed2-8560-57d7b8798375', secondary, first_formalizer_entitled_to_founder_status).
narrative_ontology:cs_axiom_status(first_formalizer_entitled_to_founder_status, holdable).
narrative_ontology:cs_axiom_grounding('58997cb6-1c10-4ed2-8560-57d7b8798375', first_formalizer_entitled_to_founder_status, instrumental).
narrative_ontology:cs_reference_frame('58997cb6-1c10-4ed2-8560-57d7b8798375', theoretical_conception_origin).
narrative_ontology:cs_drift_state('58997cb6-1c10-4ed2-8560-57d7b8798375', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('58997cb6-1c10-4ed2-8560-57d7b8798375', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_research_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, digital_cash_venture_founders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, monetary_statisticians).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_era_engineers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, economic_historians_of_adoption).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, theoretical_priority_doctrine).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, cryptographic_foundationalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cryptographers, monetary theorists, and historians of computing who maintain the field's origin narrative. They date digital money's beginning to the 1960s telecommunications advances and Chaum's 1985 blind-signature formalization, and they administer the apparatus that keeps that date canonical: peer review, graduate curricula, keynote allocation, and anniversary commemoration. Citation share, the 'inventor of digital cash' title, and the founding-chapter position in textbooks flow to this lineage. Leaving the position would mean disavowing the founding narrative that their careers, doctoral lineages, and citation capital are built on.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_research_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, academic_research_community, agenda_setter).

% The researchers-turned-entrepreneurs who converted the 1985 formalization into patents, pilot systems, and venture funding, DigiCash above all. The priority date anchored their pitch — that they held the invention of digital cash — and drew capital a decade before consumer-ready rails existed. The founders personally monetized the priority claims through patents and exits even as the ventures failed; their capital remains sunk in the digital-cash category, and leaving it means writing off that lineage.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, digital_cash_venture_founders, beneficiary,
    moderate, biographical, constrained, global).

% Central-bank and BIS statistical divisions that define and publish the monetary aggregates. Under a 1985 emergence boundary, their aggregate definitions must in principle decide what to do with formalized-but-uncirculated instruments — 'potential money' that exists as design and patent but not as holdings — a classification burden that later boundary conventions do not create. They cannot decline the task: aggregate production is mandated and the definitions are coordinated internationally.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_statisticians, payer,
    institutional, generational, constrained, global).

% The banking and telecommunications engineers who built the 1967–77 electronic payment infrastructure — ATM networks, ACH clearing, the SWIFT messaging standard. Under the thinkability boundary their work is dated before money 'emerged' and is reframed as precursor plumbing rather than founding achievement. Their professional self-concept — the societies, retirement speeches, and industry histories built around having electrified money — is constituted through the founding title the convention assigns elsewhere; they cannot exit the historiography that demotes them.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_era_engineers, payer,
    organized, biographical, identity_locked, continental).

% Scholars whose periodizations center infrastructure deployment and consumer adoption rather than theoretical formalization. The canonical boundary costs them narrative priority: their milestones are filed as 'diffusion' and their rival periodizations must argue against the default framing. Unlike the engineers, they can and do fight in print — publishing rival readings is their professional stock-in-trade — so the cost lands on citation share and framing rather than on identity.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, economic_historians_of_adoption, payer,
    moderate, biographical, mobile, global).

% The telecom-money operators — M-Pesa and its many analogues — whose digital monies emerged from airtime-credit rails in the 2000s with no connection to the cryptographic lineage the canonical boundary celebrates. The single-origin convention leaves no place for their independent emergences except as late, derivative footnotes. They stand outside the journals and curricula where the boundary is adjudicated and have no seat in that conversation; their commercial success does not depend on the historiography, but their history is written by it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, mobile_money_operators, excluded,
    powerful, biographical, constrained, regional).

% The archivists and historical units of central banks and the BIS, who hold the full documentary record — ATM deployment logs, ACH volume series, Chaum's papers and correspondence, e-purse pilot data. They take no position in the boundary dispute; their mandate is preservation and access, and their collections are the common evidence base every reading of the emergence question must eventually answer to.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_archivists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the history of digital money a single citable origin: a shared boundary lets scholars, curricula, and commemorations coordinate on when the field's story begins, and gives citation and priority attribution a common reference point across a history otherwise scattered across telecommunications engineering, banking operations, and cryptographic research.
% TRANSFER_FUNCTION: Moves founding-title authority, citation share, and narrative priority from the infrastructure-era engineers and adoption-era actors to the theoretical community of the 1960s–1985 lineage; moves measurement complexity onto monetary statisticians, whose aggregates must in principle account for formalized-but-uncirculated potential money; and moved venture legitimacy to early digital-cash founders for as long as the theory-to-circulation gap lasted.
% ABSENT_VOICES: Mobile-money operators and their user communities — whose digital monies emerged from telecom-airtime rails with no connection to the Chaum lineage — would object that the single-origin boundary renders their independent emergences invisible or derivative. So would the operations staff who ran the early ATM, ACH, and SWIFT systems and left no papers: their testimony survives mainly in institutional archives rather than in the journals where the boundary is adjudicated.
% DISAPPEARANCE_RATIONALE: If the thinkability boundary vanished overnight, the historiography would reorganize around the rival periodizations: infrastructure-era milestones would reclaim founding status, adoption-era instruments would re-enter the origin story, priority claims and the 'inventor of digital cash' title would be re-contested, and curricula and commemorative apparatus built on the 1985 date would be rewritten. The monetary-measurement question — whether aggregates track potential money — would reopen under different definitions.
% FOUNDING_PROBLEM: The history of digital money was scattered across telecommunications engineering, banking operations, and cryptographic research with no shared narrative; as the field professionalized in the 1980s–1990s it needed a citable origin to organize teaching, citation, and self-understanding. The Chaum formalization supplied one.
% FOUNDING_PROBLEM_CORROBORATION: Monetary-statistical bodies and central-bank archives, outside the benefiting academic set, attest that the pre-1985 history was organizationally scattered and needed coordination — BIS and central-bank anniversary volumes document the need directly. Infrastructure-era professional associations attest the same need while disputing the theoretical boundary. No corroboration exists from outside the beneficiary set for the specific criterion — that theoretical thinkability, rather than infrastructure or holdings, is the correct emergence test; on that point only the academic community attests, and the rival readings are the standing counter-attestation.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).
:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.44: the convention's costs are real but non-material — priority and citation rents, narrative demotion, and a measurement-classification burden — so epsilon sits mid-range rather than at coercive-institution levels. Suppression 0.48: enforcement is epistemic gatekeeping (peer review, curricula, commemoration, keynote allocation) rather than legal coercion; the mechanism is predominantly structural (institutional gatekeeping over publication and canon) with a substantial internalized component (graduate training inculcates the canonical origin, so scholars defend it without instruction) — roughly 70/30 structural to internalized, which is the split the suppression-mechanism omega would resolve precisely. Theater 0.28: the convention does genuine classificatory work; the commemorative and hagiographic share is real but a minority, and rises late in the series as the causal claim weakens and maintenance turns defensive. Accessibility_collapse 0.25: alternatives do not collapse — the sibling readings remain fully publishable and professionally viable, which is precisely why the boundary needs active enforcement instead of being self-evident. Resistance 0.55: the contest is live and organized (rival periodizations, engineering-history counterclaims). The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the gatekeeping apparatus was built up across the interval, from bare citation norms at the 1985 formalization through canon consolidation in the 1990s–2000s to a defensive ratchet after 2008, when rival origin narratives raised the stakes of holding the boundary. The late base_extractiveness dip (0.48 to 0.44) while suppression and theater rise is a coherent defensive signature: the rent pool erodes under multi-origin evidence — Nakamoto's rival founding document, telecom-money's independent emergences — exactly as enforcement intensifies and maintenance grows more performative. Identity-lock dynamics: the binding mechanism is professional identity fused with the founding narrative (careers, doctoral lineages, and citation capital built on the 1985 date) compounded by institutional identity (the field has become its origin story through curricula and commemorations); if the identity frame broke, enforcement would collapse quickly, because it is cheap only while identity holds. Same-level dynamics: academic and engineering professional communities hold equal nominal standing; what differentiates their positions under the convention is role and exit — the agenda-setting seat fused with the beneficiary seat on one side, identity-locked payers on the other, and a mobile payer seat that can defect into publishing rival readings. Boltzmann alert: identity_coordination is declared because the convention's dominant function is boundary-and-reputation coordination for a scholarly community; but the identity framing ('this is simply when our field began') is also the cover under which priority rents are collected, so the coupling test should be read with that gaming risk in mind. Here the extraction concentrates on identity-locked professional peers rather than on powerless agents at global scope — the less pathological coupling shape.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting beneficiary seat, the convention is not experienced as a constraint at all — the 1985 formalization simply is the emergence, and enforcing the boundary feels like ordinary scholarship. From the identity-locked payer seat (infrastructure-era engineers), the same convention operates as erasure: a life's work reframed as a prelude to an emergence dated after it. From the mobile payer seat (adoption historians), it operates as a contestable framing to be fought in print. The engine computes these divergent per-seat classifications from the structural data — role, power, exit — and the divergence between the beneficiary seat's rope-like experience and the identity-locked payer seat's snare-like experience of the identical arrangement is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (academic_research_community, digital_cash_venture_founders) drive d toward the beneficiary end for those seats, damping their effective extraction; the agenda-setter fusion — the academic community both administers the boundary and collects from it — is the capture shape that keeps their d low despite their enforcement role. The victim declarations (monetary_statisticians, infrastructure_era_engineers, economic_historians_of_adoption) drive d toward the target end; among them, exit differentiates magnitude — the identity-locked engineers sit nearer the full-target end than the mobile historians, whose ability to publish rival periodizations damps what they bear. The excluded seat (mobile_money_operators) is harmed without being enrolled in the arrangement: it is neither declared beneficiary nor victim, so the structural derivation under-determines its directionality — its cost is narrative erasure from outside the enforcement perimeter — and no directionality override is authored because the override mechanism keys on power atoms that other seats share. The observer seat (central_bank_archivists) sits outside the credit-allocation circuit entirely. Scope note: the convention's global spatial scope modestly amplifies effective extraction for targets, since priority claims are hardest to verify exactly at global scale — the condition under which citation rents compound. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is engine-scaled through directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   A pure-rope mislabel ('a useful periodization, nothing more') would erase the priority-rent structure and the demotion costs borne by the engineering, historical, and statistical seats; a pure-snare mislabel ('narrative theft by academics') would erase the genuine coordination function — the scattered pre-1985 history really did need an organizing origin, and the formalization really is a load-bearing node in the field's self-understanding. Tangled_rope holds both faces: the same boundary that organizes the field allocates credit asymmetrically and requires active enforcement to hold against rival periodizations. On the founding problem: organizing a scattered history is still live — every new wave of monetary technology re-raises the origin question — so the convention has not outlived its function and is not yet a piton candidate. But the drift series shows the pre-mandatrophy signature worth watching: enforcement capacity still rising while the founding narrative's causal claim weakens and maintenance grows theatrical. If the founding problem goes dead — if the field stops needing a single origin — while enforcement keeps rising, the convention completes the drift toward theatrical maintenance. On fixing cost: re-dating the emergence and re-attributing priority is prohibitive for the seat that could fix it, because the gatekeeping apparatus would be dismantling its own priority claims and rewriting curricula and commemorations its members' identities depend on, against a benefit — fairer attribution — that accrues mostly to other seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the kernel digital_money_emergence_boundary, instantiating the conceptualization_reading; what changes structurally if a sibling reading — infrastructure_reading or consumer_holdings_reading — is adopted instead?',
    'Comparative authorship of the sibling constraint stories. The disagreement is located in the emergence criterion (theoretical thinkability versus operational infrastructure versus direct consumer holdings), which propagates to the boundary date (1960s–1985 versus 1967–77 versus 1990s–2000), the beneficiary set (academic priority-claim community versus infrastructure consortia versus e-purse issuers), and the measurement consequence (only this reading forces M4/M5 to account for potential money).',
    'Adopting a sibling reading dissolves this reading''s priority-claim beneficiary structure and its potential-money measurement burden; the extraction profile relocates to whichever seat the adopted criterion favors, and this story''s victims are released or replaced accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story instantiates the conceptualization_reading; sibling readings relocate the boundary, the beneficiaries, and the measurement burden.').

omega_variable(
    single_origin_vs_multiple_emergences,
    'Does digital money have a single origin, as the boundary convention presupposes, or multiple independent emergences — telecom-airtime money, Bitcoin''s cryptographic reinvention — that a single-boundary convention cannot represent?',
    'Comparative historiography of independent reinventions: document emergence episodes with no causal lineage to the 1960s–1985 theoretical community, and test whether the convention can absorb them as diffusion or must classify them as separate emergences.',
    'If multiple emergences are accepted, the convention demotes from THE origin claim to one periodization among several; the priority-rent structure deflates, and the excluded mobile-money seat''s erasure cost becomes a named classification input rather than an under-determined residual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_origin_vs_multiple_emergences, conceptual, 'Whether the kernel''s emergence question has one answer at all.').

omega_variable(
    potential_money_measurement_burden,
    'Do monetary aggregates in practice bear a material accounting cost from the 1985 boundary — do statistical divisions actually spend effort classifying formalized-but-uncirculated instruments?',
    'Survey central-bank statistical methodology before and after 1985; compare aggregate-definition documents (BIS, ECB, national central banks) for explicit potential-money treatment and staff-time allocation.',
    'A material burden confirms monetary_statisticians as a quantifiably paying seat; a negligible burden narrows this reading''s paying structure to narrative demotion alone, lowering the measured cost asymmetry and weakening the tangled_rope reading toward a softer coordination account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_money_measurement_burden, empirical, 'Whether the potential-money accounting cost imposed by the 1985 boundary is real or nominal.').

omega_variable(
    infrastructure_causal_autonomy,
    'Was the 1967–77 electronic payment infrastructure causally dependent on the theoretical lineage this reading credits as the emergence, or was it an autonomous engineering program?',
    'Engineering-historical analysis: trace whether cryptographic-cash formalization influenced ATM, ACH, or SWIFT design decisions, or whether those systems were ledger-and-telecom engineering with no cryptographic-cash content.',
    'If autonomous, the reading''s causal priority claim is exposed as conventional rather than causal — the founding-title allocation loses its factual warrant, enforcement burden should rise as the claim is contested, and the drift toward theatrical maintenance accelerates. If dependent, the priority claim retains causal substance and the convention''s coordination function outweighs its extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_causal_autonomy, empirical, 'Whether the theoretical formalization causally enabled the infrastructure this reading dates after the emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(digi_tr_t0, observed).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(digi_tr_t10, observed).
narrative_ontology:measurement(digi_tr_t25, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(digi_tr_t25, observed).
narrative_ontology:measurement(digi_tr_t35, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(digi_tr_t35, observed).
narrative_ontology:measurement(digi_tr_t45, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement_basis(digi_tr_t45, observed).
narrative_ontology:measurement(digi_tr_t55, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 55, 0.25).
narrative_ontology:measurement_basis(digi_tr_t55, observed).
narrative_ontology:measurement(digi_tr_t65, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 65, 0.28).
narrative_ontology:measurement_basis(digi_tr_t65, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(digi_be_t0, observed).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(digi_be_t10, observed).
narrative_ontology:measurement(digi_be_t25, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement_basis(digi_be_t25, observed).
narrative_ontology:measurement(digi_be_t35, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement_basis(digi_be_t35, observed).
narrative_ontology:measurement(digi_be_t45, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 45, 0.46).
narrative_ontology:measurement_basis(digi_be_t45, observed).
narrative_ontology:measurement(digi_be_t55, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 55, 0.48).
narrative_ontology:measurement_basis(digi_be_t55, observed).
narrative_ontology:measurement(digi_be_t65, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 65, 0.44).
narrative_ontology:measurement_basis(digi_be_t65, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(digi_su_t0, observed).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement_basis(digi_su_t10, observed).
narrative_ontology:measurement(digi_su_t25, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(digi_su_t25, observed).
narrative_ontology:measurement(digi_su_t35, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 35, 0.3).
narrative_ontology:measurement_basis(digi_su_t35, observed).
narrative_ontology:measurement(digi_su_t45, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement_basis(digi_su_t45, observed).
narrative_ontology:measurement(digi_su_t55, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 55, 0.44).
narrative_ontology:measurement_basis(digi_su_t55, observed).
narrative_ontology:measurement(digi_su_t65, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 65, 0.48).
narrative_ontology:measurement_basis(digi_su_t65, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, identity_coordination).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes, per the epsilon-invariance principle, into three structurally distinct claims: thinkability (this file), operational infrastructure (infrastructure_reading), and direct consumer holdings (consumer_holdings_reading). Each carries its own epsilon, beneficiary structure, and measurement consequences — only this reading forces M4/M5 to account for potential money, and only this reading concentrates beneficiaries in the academic priority-claim community. They form a constraint family linked through these edges; this story is the family's upstream member, and its priority-claim apparatus is cited by downstream proponents when arguing precedence, which is why the edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
